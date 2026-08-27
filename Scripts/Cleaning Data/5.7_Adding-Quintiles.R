#### Script to add the income quintiles 

## -----------------------------
# 6b. Add income quintile columns
# -----------------------------

#merged_energy_weather_hi_2019 <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/April Complete Reset/Data/Data-Output/Legit Data Output/5.7_clean_summer2019_HI_geoid_weather.csv")
#merged_energy_weather_hi_2019 <- merged_energy_weather_hi_2019

quintile_cuts <- quantile(
  merged_energy_weather_hi_2019$median_hh_income, 
  probs = seq(0, 1, 0.2), na.rm = TRUE
)

merged_energy_weather_hi_2019 <- merged_energy_weather_hi_2019 %>%
  mutate(
    income_quintile = cut(
      median_hh_income,
      breaks = quintile_cuts,
      include.lowest = TRUE,
      labels = c("Q1: 0-20%", "Q2: 20-40%", "Q3: 40-60%", "Q4: 60-80%", "Q5: 80-100%")
    ),
    income_bucket = as.integer(income_quintile)
  )

# Sanity check
merged_energy_weather_hi_2019 %>%
  count(income_quintile, income_bucket) %>%
  arrange(income_bucket)

# Re-save
out_path <- "~/Library/Mobile Documents/com~apple~CloudDocs/April Complete Reset/Data/Data-Output/Legit Data Output/5.7_clean_merged_weather_quintiles_2019.csv"
write_csv(merged_energy_weather_hi_2019, out_path)