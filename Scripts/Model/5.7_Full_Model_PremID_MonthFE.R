### --- Libraries ---
library(tidyverse)
library(fixest)
library(segmented)
library(lubridate)
library(scales)
library(modelsummary)
library(showtext)
library(flextable)

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
merged_weather_2019_quint <- read_csv(
  "~/Library/Mobile Documents/com~apple~CloudDocs/April Complete Reset/Data/Data-Output/Legit Data Output/5.7_clean_merged_weather_quintiles_2019.csv"
) %>%
  filter(Daily_Consumption > 0) %>%
  mutate(
    DATE            = ymd(DATE),
    HI_F_new        = HI_F_New,
    log_consumption = log(Daily_Consumption),
    month           = factor(lubridate::month(DATE, label = TRUE, abbr = TRUE),
                             levels = c("May","Jun","Jul","Aug","Sep")),
    income_quintile = factor(
      income_quintile,
      levels = c("Q1: 0-20%", "Q2: 20-40%", "Q3: 40-60%", "Q4: 60-80%", "Q5: 80-100%")
    )
  )

# --- 2. Aggregate pattern (for visualization) ---
avg_by_hi <- merged_weather_2019_quint %>%
  group_by(HI_F_new) %>%
  summarise(
    mean_log_consumption = mean(log_consumption, na.rm = TRUE),
    n_households         = n_distinct(PREM_ID),
    .groups = "drop"
  )

# --- 3. Davies test & segmented regression ---
lm_fit <- lm(mean_log_consumption ~ HI_F_new, data = avg_by_hi)
davies.test(lm_fit, ~HI_F_new)
seg_fit    <- segmented(lm_fit, seg.Z = ~HI_F_new, psi = 100)
bp         <- seg_fit$psi[2]
slope_vals <- slope(seg_fit)$HI_F_new[1, ]
slope_below <- round(slope_vals[1], 3)
slope_above <- round(slope_vals[2], 3)

cat("Breakpoint:", round(bp, 2), "°F\n")

# --- 4. Add piecewise terms & run FE model ---
merge_ewhi_ready <- merged_weather_2019_quint %>%
  mutate(
    HI_below = pmin(HI_F_new, bp),
    HI_above = pmax(0, HI_F_new - bp)
  )

fe_fit <- feols(
  log_consumption ~ HI_below + HI_above + Precipitation +
    I(Precipitation^2) + Wind_Speed | PREM_ID + month,
  cluster = ~PREM_ID,
  data = merge_ewhi_ready
)
summary(fe_fit)

# --- 5. Prediction grid (for plot alignment) ---
mu_precip <- mean(merge_ewhi_ready$Precipitation, na.rm = TRUE)
mu_wind   <- mean(merge_ewhi_ready$Wind_Speed,    na.rm = TRUE)

pred_grid <- tibble(
  HI_F_new = seq(min(merge_ewhi_ready$HI_F_new, na.rm = TRUE),
                 max(merge_ewhi_ready$HI_F_new, na.rm = TRUE),
                 length.out = 200)
) %>%
  mutate(
    HI_below      = pmin(HI_F_new, bp),
    HI_above      = pmax(0, HI_F_new - bp),
    Precipitation = mu_precip,
    Wind_Speed    = mu_wind,
    PREM_ID       = merge_ewhi_ready$PREM_ID[1],
    month         = merge_ewhi_ready$month[1]
  )

pred_grid$fit <- predict(fe_fit, newdata = pred_grid)
offset        <- mean(avg_by_hi$mean_log_consumption, na.rm = TRUE) -
  mean(pred_grid$fit, na.rm = TRUE)
pred_grid$fit <- pred_grid$fit + offset

# --- 6. Confidence ribbon from segmented fit ---
pred_df <- data.frame(
  HI_F_new = seq(min(avg_by_hi$HI_F_new), max(avg_by_hi$HI_F_new), length.out = 200)
)
pred          <- predict(seg_fit, newdata = pred_df, se.fit = TRUE)
pred_df$fit   <- pred$fit
pred_df$lwr   <- pred$fit - 1.96 * pred$se.fit
pred_df$upr   <- pred$fit + 1.96 * pred$se.fit

coef_below <- round(coef(fe_fit)["HI_below"], 3)
coef_above <- round(coef(fe_fit)["HI_above"], 3)

# --- 7. Plot ---
ggplot(avg_by_hi, aes(x = HI_F_new, y = mean_log_consumption)) +
  geom_point(aes(size = n_households), color = "grey50", alpha = 0.4) +
  geom_ribbon(
    data = pred_df,
    aes(x = HI_F_new, ymin = lwr, ymax = upr),
    inherit.aes = FALSE,
    fill = "#d73027", alpha = 0.18
  ) +
  geom_line(
    data = pred_df,
    aes(y = fit),
    color = "#d73027", linewidth = 1.2
  ) +
  geom_vline(xintercept = bp, linetype = "dashed", color = "#4C67A1", linewidth = 0.7) +
  annotate("text",
           x = bp - 5,
           y = max(avg_by_hi$mean_log_consumption, na.rm = TRUE) - 0.15,
           label = paste0("Slope below: ", coef_below),
           hjust = 1, vjust = 1.5, color = "grey20", size = 4.5) +
  annotate("text",
           x = bp + 5,
           y = max(avg_by_hi$mean_log_consumption, na.rm = TRUE) - 0.15,
           label = paste0("Slope above: ", coef_above),
           hjust = 0, vjust = 1.5, color = "grey20", size = 4.5) +
  labs(
    caption = paste0("Estimated breakpoint: ", round(bp, 2), "°F"),
    x       = "Heat Index (°F)",
    y       = "Average log electricity consumption (kWh)"
  ) +
  scale_size_continuous(range = c(1.5, 6), guide = "none") +
  coord_cartesian(ylim = c(2, 5.5), xlim = c(80, 115)) +
  theme_nature()

ggsave("baseline_segmented.png", width = 10, height = 6, dpi = 300)

# --- 8. Regression table ---

modelsummary(
  fe_fit,
  output = "regression_table.docx",
  fmt = 6,
  stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
  coef_rename = c(
    "HI_below"           = "Heat Index (below threshold)",
    "HI_above"           = "Heat Index (above threshold)",
    "Precipitation"      = "Precipitation",
    "I(Precipitation^2)" = "Precipitation²",
    "Wind_Speed"         = "Wind Speed"
  ),
  gof_map = c("nobs", "r.squared", "adj.r.squared", "rmse"),
  title = "Fixed Effects Regression: Log Daily Electricity Consumption",
  notes = list(
    "Standard errors clustered by household (PREM_ID)",
    "Fixed effects: Household and Month"
  )
)

n_distinct(merged_weather_2019_quint$PREM_ID)

merged_weather_2019_quint %>%
  group_by(income_quintile) %>%
  summarise(n_households = n_distinct(PREM_ID))
