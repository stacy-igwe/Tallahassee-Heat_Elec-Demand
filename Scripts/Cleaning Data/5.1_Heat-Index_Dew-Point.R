####Calculation for Heat Index + Inclusion of Dew Point Data 
#### Create GEOID weather with dew-point-based Heat Index
#### Then merge onto 5.1_clean_153_day_merged_2019.csv

library(tidyverse)
library(lubridate)
library(data.table)

# -----------------------------
# 1. Load cleaned energy data
# -----------------------------
energy_2019 <- read_csv(
  "~/Library/Mobile Documents/com~apple~CloudDocs/April Complete Reset/Data/Data-Output/Legit Data Output/5.1_clean_153_day_merged_2019.csv",
  show_col_types = FALSE
) %>%
  mutate(DATE = as.Date(DATE))

# -----------------------------
# 2. Load GEOID weather data
# -----------------------------
geoid_weather_2019 <- read_csv(
  "~/Library/Mobile Documents/com~apple~CloudDocs/April Complete Reset/Data/Original Files/Weather_Census_Data/GEOID_2WEATHER_DEMO_2019.csv",
  show_col_types = FALSE
) %>%
  mutate(
    DATE = as.Date(DATE),
    TMAX_C = Max_Temp - 273.15,
    TMAX_F = TMAX_C * 9/5 + 32,
    TMIN_C = Min_Temp - 273.15,
    TMIN_F = TMIN_C * 9/5 + 32
  )

# -----------------------------
# 3. Load 2019 dew point data
# -----------------------------
dew_2019 <- read_csv(
  "~/Library/Mobile Documents/com~apple~CloudDocs/April Complete Reset/Data/Original Files/Dew_Point_Data/tallahassee_daily_dewpoint_2019 copy.csv",
  show_col_types = FALSE
) %>%
  mutate(
    DATE = as.Date(date),
    dewpoint_F = dewpoint_C * 9/5 + 32
  ) %>%
  select(DATE, dewpoint_C, dewpoint_F)

# -----------------------------
# 4. Define RH + Heat Index functions
# -----------------------------
calc_rh <- function(T_C, Td_C) {
  e_T  <- 6.11 * 10^(7.5 * T_C / (237.3 + T_C))
  e_Td <- 6.11 * 10^(7.5 * Td_C / (237.3 + Td_C))
  RH <- 100 * (e_Td / e_T)
  pmin(pmax(RH, 0), 100)
}

heat_index_rothfusz <- function(T_F, RH) {
  if (is.na(T_F) | is.na(RH)) return(NA_real_)
  
  HI <- -42.379 + 
    2.04901523 * T_F + 
    10.14333127 * RH - 
    0.22475541 * T_F * RH - 
    0.00683783 * T_F^2 - 
    0.05481717 * RH^2 + 
    0.00122874 * T_F^2 * RH + 
    0.00085282 * T_F * RH^2 - 
    0.00000199 * T_F^2 * RH^2
  
  if (RH < 13 && T_F >= 80 && T_F <= 112) {
    adjustment <- ((13 - RH) / 4) * sqrt((17 - abs(T_F - 95)) / 17)
    HI <- HI - adjustment
  } else if (RH > 85 && T_F >= 80 && T_F <= 87) {
    adjustment <- ((RH - 85) / 10) * ((87 - T_F) / 5)
    HI <- HI + adjustment
  }
  
  simple_HI <- 0.5 * (T_F + 61.0 + ((T_F - 68.0) * 1.2) + (RH * 0.094))
  avg_HI <- (simple_HI + T_F) / 2
  
  if (avg_HI < 80) return(avg_HI) else return(HI)
}

# -----------------------------
# 5. Add dew point + Heat Index to GEOID weather
# -----------------------------
geoid_weather_hi_2019 <- geoid_weather_2019 %>%
  left_join(dew_2019, by = "DATE") %>%
  mutate(
    RH_dewpoint = calc_rh(TMAX_C, dewpoint_C),
    HI_F_New = mapply(heat_index_rothfusz, TMAX_F, RH_dewpoint)
  )

names(energy_2019)

# Optional check
geoid_weather_hi_2019 %>%
  summarise(
    rows = n(),
    geoids = n_distinct(GEOID),
    unique_dates = n_distinct(DATE),
    missing_dewpoint = sum(is.na(dewpoint_C)),
    missing_HI = sum(is.na(HI_F_New))
  )

# -----------------------------
# 6. Merge weather + HI onto cleaned energy data
# -----------------------------
merged_energy_weather_hi_2019 <- energy_2019 %>%
  left_join(geoid_weather_hi_2019, by = c("GEOID", "DATE"))

names(energy_2019)
# Final check
merged_energy_weather_hi_2019 %>%
  summarise(
    rows = n(),
    unique_premises = n_distinct(PREM_ID),
    unique_accounts = n_distinct(ACCT_ID),
    min_date = min(DATE, na.rm = TRUE),
    max_date = max(DATE, na.rm = TRUE),
    unique_dates = n_distinct(DATE),
    missing_HI = sum(is.na(HI_F_New))
  )

# -----------------------------
# 7. Save final merged file
# -----------------------------
out_path <- "~/Library/Mobile Documents/com~apple~CloudDocs/April Complete Reset/Data/Data-Output/5.7_clean_summer2019_HI_geoid_weather.csv"

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

write_csv(merged_energy_weather_hi_2019, out_path)

head(merged_energy_weather_hi_2019)
