### --- Libraries ---
library(tidyverse)
library(fixest)
library(segmented)
library(ggplot2)
library(purrr)
library(broom)
library(lubridate)
library(scales)
library(modelsummary)
library(pandoc)
library(flextable)
library(broom)

### =========================================================
###  PART 1: BASELINE SEGMENTED MODEL (with MONTH FEs)
### =========================================================
library(showtext)

font_add_google("Lato", "lato")
showtext_auto()

theme_nature <- function() {
  theme_minimal(base_family = "lato") +
    theme(
      text = element_text(size = 14, color = "black"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 12),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      plot.margin = margin(12, 15, 12, 15),
      legend.position = "bottom",
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 11),
      legend.key.size = unit(0.9, "lines")
    )
}

# --- 1. Load & prep data ---
merge_ewhi_2019 <-read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/April Complete Reset/Data/Data-Output/Legit Data Output/5.12_HI_clean_merged_weather_quintiles_2019_CORRECT.csv")

#  merged_weather_2019_quint <- merge_ewhi_2019 %>%
#    filter(summer_status == "complete", Daily_Consumption > 0) %>%
#   mutate(
#      DATE = ymd(DATE),
#      MONTH = factor(MONTH(DATE, label = TRUE, abbr = TRUE)),
#      log_consumption = log(Daily_Consumption)
#    )


merged_weather_2019_quint <- merge_ewhi_2019
# --- 2. Aggregate pattern (for visualization) ---
avg_by_hi <- merged_weather_2019_quint %>%
  group_by(HI_F_New) %>%
  summarise(
    mean_log_consumption = mean(log_consumption, na.rm = TRUE),
    n_households = n_distinct(PREM_ID),
    .groups = "drop"
  )

# --- 3. Davies test & segmented regression ---
lm_fit <- lm(mean_log_consumption ~ HI_F_New, data = avg_by_hi)
davies.test(lm_fit, ~HI_F_New)
seg_fit <- segmented(lm_fit, seg.Z = ~HI_F_New, psi = 100)

bp <- seg_fit$psi[2]
slope_vals <- slope(seg_fit)$HI_F_New[1,]
slope_below <- round(slope_vals[1], 3)
slope_above <- round(slope_vals[2], 3)

# --- 4. Add piecewise terms & run FE model (household + MONTH FE) ---
merge_ewhi_ready <- merged_weather_2019_quint %>%
  mutate(
    HI_below = pmin(HI_F_New, bp),
    HI_above = pmax(0, HI_F_New - bp)
  )

fe_fit <- feols(
  log_consumption ~ HI_below + HI_above + Precipitation +
    I(Precipitation^2) + Wind_Speed | PREM_ID + MONTH,
  cluster = ~PREM_ID,
  data = merge_ewhi_ready
)
summary(fe_fit)

# --- 5. Prediction grid ---
mu_precip <- mean(merge_ewhi_ready$Precipitation, na.rm = TRUE)
mu_wind   <- mean(merge_ewhi_ready$Wind_Speed,   na.rm = TRUE)

pred_grid <- tibble(
  HI_F_New = seq(min(merge_ewhi_ready$HI_F_New, na.rm = TRUE),
                 max(merge_ewhi_ready$HI_F_New, na.rm = TRUE),
                 length.out = 200)
) %>%
  mutate(
    HI_below = pmin(HI_F_New, bp),
    HI_above = pmax(0, HI_F_New - bp),
    Precipitation = mu_precip,
    Wind_Speed = mu_wind,
    PREM_ID = merge_ewhi_ready$PREM_ID[1],
    MONTH = merge_ewhi_ready$MONTH[1]
  )

pred_grid$fit <- predict(fe_fit, newdata = pred_grid)
offset <- mean(avg_by_hi$mean_log_consumption, na.rm = TRUE) -
  mean(pred_grid$fit, na.rm = TRUE)
pred_grid$fit <- pred_grid$fit + offset

# --- 6. Plot (aggregate segmented fit) ---
pred_df <- data.frame(
  HI_F_New = seq(min(avg_by_hi$HI_F_New), max(avg_by_hi$HI_F_New), length.out = 200)
)
pred <- predict(seg_fit, newdata = pred_df, se.fit = TRUE)
pred_df$fit <- pred$fit
pred_df$lwr <- pred$fit - 1.96 * pred$se.fit
pred_df$upr <- pred$fit + 1.96 * pred$se.fit

coef_below <- round(coef(fe_fit)["HI_below"], 3)
coef_above <- round(coef(fe_fit)["HI_above"], 3)

ok <- ggplot(avg_by_hi, aes(x = HI_F_New, y = mean_log_consumption)) +
  
  # Binned averages
  geom_point(
    aes(size = n_households),
    color = "grey50",
    alpha = 0.4
  ) +
  
  # Confidence ribbon
  geom_ribbon(
    data = pred_df,
    aes(x = HI_F_New, ymin = lwr, ymax = upr),
    inherit.aes = FALSE,
    fill = "#d73027",
    alpha = 0.18
  ) +
  
  # Fitted segmented line
  geom_line(
    data = pred_df,
    aes(y = fit),
    color = "#d73027",
    linewidth = 1.2
  ) +
  
  # Breakpoint
  geom_vline(
    xintercept = bp,
    linetype = "dashed",
    color = "#4C67A1",
    linewidth = 0.7
  ) +
  
  # Annotations (subtle)
  annotate(
    "text",
    x = bp - 5,
    y = max(avg_by_hi$mean_log_consumption, na.rm = TRUE) - 0.15,
    label = paste0("Slope below: ", coef_below),
    hjust = 1,
    vjust = 1.5,
    color = "grey20",
    size = 4.5
  ) +
  annotate(
    "text",
    x = bp + 5,
    y = max(avg_by_hi$mean_log_consumption, na.rm = TRUE) - 0.15,
    label = paste0("Slope above: ", coef_above),
    hjust = 0,
    vjust = 1.5,
    color = "grey20",
    size = 4.5
  ) +
  
  # labs(
  #   #title = "Heat index and average residential electricity consumption",
  #   subtitle = paste0("Estimated breakpoint = ", round(bp, 2), "°F"),
  #   caption = paste0(
  #     "Sample: ",
  #     scales::comma(length(unique(merged_weather_2019_quint$PREM_ID))),
  #     " premises"
  #   ),
  #   x = "Heat Index (°F)",
  #   y = "Average log electricity consumption"
  # ) +
  
  labs(
    #title = "Heat index and average residential electricity consumption",
    #subtitle = paste0("Estimated breakpoint = ", round(bp, 2), "°F"),
    caption = paste0(
      "Estimated breakpoint: ",
      round(bp, 2), "°F"
    ),
    x = "Heat Index (°F)",
    y = "Average log electricity consumption (kWh)"
  ) +
  
  scale_size_continuous(range = c(1.5, 6), guide = "none") +
  coord_cartesian(ylim = c(2, 5.5), xlim = c(80,115)) +
  
  theme_nature()

ok

summary(fe_fit)


modelsummary(
  fe_fit,
  output = "regression_table.docx",
  stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
  coef_rename = c(
    "HI_below" = "Heat Index (below threshold)",
    "HI_above" = "Heat Index (above threshold)",
    "Precipitation" = "Precipitation",
    "I(Precipitation^2)" = "Precipitation²",
    "Wind_Speed" = "Wind Speed"
  ),
  gof_map = c("nobs", "r.squared", "adj.r.squared", "rmse"),
  title = "Fixed Effects Regression: Log Daily Electricity Consumption",
  notes = list("Standard errors clustered by household (PREM_ID)",
               "Fixed effects: Household and MONTH")
)
library(flextable)
library(broom)

# Create tidy table
tidy_table <- data.frame(
  Variable = c("Heat Index (below threshold)", "Heat Index (above threshold)", 
               "Precipitation", "Precipitation²", "Wind Speed"),
  Estimate = c(0.0194, 0.0092, -0.0093, 0.0002, 0.0005),
  `Std. Error` = c(0.00004, 0.00009, 0.00004, 0.000001, 0.0001),
  `t value` = c(449.10, 97.92, -232.13, 144.87, 4.90),
  `p value` = c("<0.001", "<0.001", "<0.001", "<0.001", "<0.001")
)

library(stargazer)

stargazer(fe_fit, 
          type = "html",
          out = "regression_table.html",
          title = "Fixed Effects Regression Results",
          dep.var.labels = "Log Daily Consumption",
          covariate.labels = c("Heat Index (below)", "Heat Index (above)",
                               "Precipitation", "Precipitation²", "Wind Speed"))
# Then open HTML in Word and save as .docx
### =========================================================
###  PART 2: INCOME-QUINTILE MODEL (with MONTH FEs)
### =========================================================
### --- Libraries ---
library(tidyverse)
library(fixest)
library(segmented)
library(purrr)
library(broom)
library(scales)

### --- 1. Load and prep data ---
merge_ewhi_2019_quint <- read_csv(
  "Data-Output/2019 Data/0. Part 2 Analysis/merged_weather_2019_newHI_summer_selected_pw_quintiles.csv"
) %>%
  filter(summer_status == "complete", Daily_Consumption > 0) %>%
  mutate(
    DATE = ymd(DATE),
    MONTH = factor(MONTH(DATE, label = TRUE, abbr = TRUE)),  # closed parenthesis here
    log_consumption = log(Daily_Consumption),
    income_quintile = factor(
      income_quintile,
      levels = c("Q1: Lowest 20%", "Q2: 20-40%", "Q3: 40-60%",
                 "Q4: 60-80%", "Q5: Highest 20%")
    )
  )


### --- 2. Find breakpoints per quintile ---
find_breakpoint <- function(df, group_name) {
  lm_fit <- lm(log_consumption ~ HI_F_New, data = df)
  dav <- davies.test(lm_fit, seg.Z = ~HI_F_New)
  seg_fit <- tryCatch(segmented(lm_fit, seg.Z = ~HI_F_New, psi = 100), error = function(e) NULL)
  
  tibble(
    income_quintile = group_name,
    breakpoint = if (!is.null(seg_fit)) seg_fit$psi[2] else NA_real_,
    davies_p = dav$p.value
  )
}

breakpoints_by_quint <- merge_ewhi_2019_quint %>%
  group_split(income_quintile) %>%
  map_df(~find_breakpoint(.x, unique(.x$income_quintile)))

print(breakpoints_by_quint)

### --- 3. Add HI_below / HI_above terms and run FE model ---
merge_ewhi_2019_quint <- merge_ewhi_2019_quint %>%
  left_join(breakpoints_by_quint, by = "income_quintile") %>%
  mutate(
    HI_below = pmin(HI_F_New, breakpoint),
    HI_above = pmax(0, HI_F_New - breakpoint)
  )

fe_fit_income <- feols(
  log_consumption ~ HI_below:income_quintile + HI_above:income_quintile +
    Precipitation + I(Precipitation^2) + Wind_Speed |
    PREM_ID + MONTH,
  cluster = ~PREM_ID,
  data = merge_ewhi_2019_quint
)
summary(fe_fit_income)

### --- 4. Wald tests (slope equality across quintiles) ---
# Below-break slopes: test Q1 vs Q5
# Below-break test
# Fix these lines (remove extra quote)
# Using fixest::wald
wald(fe_fit_income, c("`HI_below:income_quintileQ1: Lowest 20%`" = "`HI_below:income_quintileQ5: Highest 20%`"))

library(aod)

b <- coef(fe_fit_income)
V <- vcov(fe_fit_income)
cn <- names(b)

# Verify names exist
coefs_above <- c(
  "income_quintileQ1: Lowest 20%:HI_above",
  "income_quintileQ2: 20-40%:HI_above",
  "income_quintileQ3: 40-60%:HI_above",
  "income_quintileQ4: 60-80%:HI_above",
  "income_quintileQ5: Highest 20%:HI_above"
)

coefs_below <- c(
  "HI_below:income_quintileQ1: Lowest 20%",
  "HI_below:income_quintileQ2: 20-40%",
  "HI_below:income_quintileQ3: 40-60%",
  "HI_below:income_quintileQ4: 60-80%",
  "HI_below:income_quintileQ5: Highest 20%"
)

# Check if names match
print(coefs_above %in% cn)
print(coefs_below %in% cn)

# Build contrast matrices
L <- t(sapply(coefs_above[1:4], function(x) {
  as.numeric(cn == x) - as.numeric(cn == coefs_above[5])
}))

M <- t(sapply(coefs_below[1:4], function(x) {
  as.numeric(cn == x) - as.numeric(cn == coefs_below[5])
}))

# Run Wald tests
wald.test(b = b, Sigma = V, L = L)  # above-break
wald.test(b = b, Sigma = V, L = M)  # below-break

### --- 5. Prediction grid for all quintiles ---
### --- 5. Prediction grid for all quintiles (no FE required) ---
# --- 5. Prediction grid for all quintiles ---
# choose any valid PREM_ID and MONTH that exist in the training data
ref_prem  <- unique(merge_ewhi_2019_quint$PREM_ID)[1]
ref_MONTH <- unique(merge_ewhi_2019_quint$MONTH)[1]

pred_grid <- expand.grid(
  HI_F_New        = seq(80, 115, by = 0.5),
  income_quintile = levels(merge_ewhi_2019_quint$income_quintile)
) %>%
  left_join(breakpoints_by_quint, by = "income_quintile") %>%
  mutate(
    HI_below      = pmin(HI_F_New, breakpoint),
    HI_above      = pmax(0, HI_F_New - breakpoint),
    Precipitation = mean(merge_ewhi_2019_quint$Precipitation, na.rm = TRUE),
    Wind_Speed    = mean(merge_ewhi_2019_quint$Wind_Speed, na.rm = TRUE),
    PREM_ID       = ref_prem,   # always include valid FE values
    MONTH         = ref_MONTH
  )

# explicitly ensure both FE vars are in newdata, then predict
pred_grid$fit <- as.numeric(
  predict(fe_fit_income, newdata = pred_grid, fixef = FALSE)
)

### --- 6. Label income groups by average median income ---
avg_median_incomes <- merge_ewhi_2019_quint %>%
  group_by(income_quintile) %>%
  summarise(avg_income = mean(median_hh_income, na.rm = TRUE)) %>%
  mutate(label = paste0(income_quintile, ": $", comma(round(avg_income))))

pred_grid <- pred_grid %>%
  left_join(avg_median_incomes, by = "income_quintile") %>%
  mutate(label = factor(label, levels = avg_median_incomes$label))

breakpoints_by_quint <- left_join(breakpoints_by_quint, avg_median_incomes, by = "income_quintile")

### --- 7. Final quintile plot ---
library(ggplot2)
library(dplyr)

library(RColorBrewer)
brewer.pal(5, "Set1")

# Clean up labels for clarity
label_map <- c(
  "Q1: Lowest 20%: $10,482"   = "Q1: Lowest 20%",
  "Q2: 20-40%: $20,535"       = "Q2: 20-40%",
  "Q3: 40-60%: $29,479"       = "Q3: 40-60%",
  "Q4: 60-80%: $37,314"       = "Q4: 60-80%",
  "Q5: Highest 20%: $50,445"  = "Q5: Highest 20%"
)

# Apply label mapping
# Step 1: Add cleaned label to pred_grid
# Recode labels
pred_grid <- pred_grid %>%
  mutate(label_clean = recode(label, !!!label_map))

breakpoints_by_quint <- breakpoints_by_quint %>%
  mutate(label_clean = recode(label, !!!label_map))

# Interpolate predicted fit at each breakpoint
breakpoints_by_quint <- breakpoints_by_quint %>%
  rowwise() %>%
  mutate(fit = approx(
    x = pred_grid$HI_F_New[pred_grid$label_clean == label_clean],
    y = pred_grid$fit[pred_grid$label_clean == label_clean],
    xout = breakpoint
  )$y) %>%
  ungroup()


# Line types for quintiles
line_types <- c("solid", "dashed", "dotdash", "twodash", "longdash")

# Final plot
ggplot(pred_grid, aes(x = HI_F_New, y = fit, group = label_clean, linetype = label_clean)) +
  
  # Quintile lines in black, different linetypes
  geom_line(color = "black", linewidth = 1.1) +
  
  
  # Breakpoint markers
  geom_point(
    data = breakpoints_by_quint,
    aes(x = breakpoint, y = fit, fill = label_clean),
    shape = 21,
    size = 4,
    stroke = 1,
    alpha = .75,
    color = "black",
    show.legend = FALSE
  ) +
  
  
  # Manually assign line types
  scale_linetype_manual(values = line_types) +
  
  # Axis labels and plot text
  labs(
    title = "Predicted Log Daily Electricity Use by Income Quintile and Heat Index",
    subtitle = "~80,000 premises, FE-adjusted predictions with breakpoint markers",
    x = "Heat Index (°F)",
    y = "Predicted Log of Daily Electricity Consumption (kWh)",
    linetype = "Income Group"
  ) +
  
  coord_cartesian(ylim = c(1.8, 2.8), xlim = c(80, 115)) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 5)),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )
