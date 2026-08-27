### ============================================================
###   SEGMENTED FE BY INCOME QUINTILE - REBATE HOUSEHOLDS ONLY
### ============================================================

library(tidyverse)
library(fixest)
library(segmented)
library(lubridate)
library(scales)
library(aod)
library(readr)
library(readxl)
library(showtext)
library(flextable)

font_add_google("Lato", "lato")
showtext_auto()

theme_nature_r <- function() {
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

### --- 1. Load data ---
df_r <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/April Complete Reset/Data/Data-Output/Legit Data Output/5.12_HI_clean_merged_weather_quintiles_2019_CORRECT.csv")

rebate_hh_r <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/April Complete Reset/Data/Original Files/Rebate Data/5.11_Tallahassee_Rebates_2011_2018.xlsx")

rebate_hh_filter_r <- rebate_hh_r %>%
  filter(ADJ_TYPE_CD %in% c('ES-HP15', 'ES-AC15', 'ES-WSHP', 'ESR-HP',
                            'ES-AC14', 'ES-HP14', 'ES-HPESP', 'ES-ACESP',
                            'ES-ACES', 'ES-HPES'))

list_rebate_hh_r <- unique(rebate_hh_filter_r$PREM_ID)

### --- 2. Filter to rebate households only ---
rebate_with_quint_r <- df_r %>%
  filter(PREM_ID %in% list_rebate_hh_r, Daily_Consumption > 0) %>%
  mutate(
    DATE            = ymd(DATE),
    HI_F_new_r      = HI_F_New,
    log_consumption = log(Daily_Consumption),
    MONTH           = factor(lubridate::month(DATE, label = TRUE, abbr = TRUE),
                             levels = c("May","Jun","Jul","Aug","Sep")),
    income_quintile = factor(
      income_quintile,
      levels = c("Q1: 0-20%", "Q2: 20-40%", "Q3: 40-60%", "Q4: 60-80%", "Q5: 80-100%")
    )
  )

cat("=== REBATE HOUSEHOLDS BY INCOME QUINTILE ===\n")
rebate_with_quint_r %>%
  group_by(income_quintile) %>%
  summarise(n_obs = n(), n_hh = n_distinct(PREM_ID)) %>%
  print()
cat("\nTotal rebate households:", n_distinct(rebate_with_quint_r$PREM_ID), "\n")

out_dir_r <- "~/Library/Mobile Documents/com~apple~CloudDocs/April Complete Reset/Images"

### ============================================================
###   VERSION 1: SEPARATE MODELS (5 models, 5 breakpoints)
### ============================================================

run_separate_model_r <- function(df_subset_r, group_name_r) {
  lm_fit_r  <- lm(log_consumption ~ HI_F_new_r, data = df_subset_r)
  seg_fit_r <- tryCatch(
    segmented(lm_fit_r, seg.Z = ~HI_F_new_r, psi = 100),
    error = function(e) NULL
  )
  bp_r  <- if (!is.null(seg_fit_r)) seg_fit_r$psi[2] else 104
  dav_r <- tryCatch(davies.test(lm_fit_r, seg.Z = ~HI_F_new_r),
                    error = function(e) list(p.value = NA))
  
  df_subset_r <- df_subset_r %>%
    mutate(HI_below_r = pmin(HI_F_new_r, bp_r), HI_above_r = pmax(0, HI_F_new_r - bp_r))
  
  fe_fit_r <- feols(
    log_consumption ~ HI_below_r + HI_above_r +
      Precipitation + I(Precipitation^2) + Wind_Speed |
      PREM_ID + MONTH,
    cluster = ~PREM_ID, data = df_subset_r
  )
  
  list(quintile = group_name_r, breakpoint = bp_r, davies_p = dav_r$p.value,
       model = fe_fit_r, n_hh = n_distinct(df_subset_r$PREM_ID), n_obs = nrow(df_subset_r))
}

results_separate_r <- rebate_with_quint_r %>%
  group_split(income_quintile) %>%
  map(~run_separate_model_r(.x, as.character(unique(.x$income_quintile))))

separate_summary_r <- map_df(results_separate_r, function(r) {
  coefs_r <- coef(r$model); ses_r <- sqrt(diag(vcov(r$model)))
  tibble(
    income_quintile = r$quintile, n_hh = r$n_hh,
    breakpoint  = round(r$breakpoint, 2), davies_p = r$davies_p,
    slope_below = round(coefs_r["HI_below_r"], 4), se_below = round(ses_r["HI_below_r"], 4),
    slope_above = round(coefs_r["HI_above_r"], 4), se_above = round(ses_r["HI_above_r"], 4)
  )
})

sink(file.path(out_dir_r, "rebate_separate_models_results.txt"))
cat("\n=== REBATE SEPARATE MODELS SUMMARY ===\n")
print(separate_summary_r)
for (r in results_separate_r) {
  cat("\n===", r$quintile, "===\n")
  cat("Breakpoint:", round(r$breakpoint, 4), "°F | Households:", r$n_hh, "\n")
  print(summary(r$model))
}
sink()

### ============================================================
###   SAVE & PURGE BEFORE POOLED MODEL
### ============================================================

write_csv(separate_summary_r, file.path(out_dir_r, "rebate_separate_summary.csv"))

loglik_separate_r <- map_dbl(results_separate_r, ~logLik(.x$model)[1])
saveRDS(loglik_separate_r, file.path(out_dir_r, "rebate_loglik_separate.rds"))

# Save lightweight coefs for separate plot (robust regex match for Precipitation^2)
plot_coefs_separate_r <- map_df(results_separate_r, function(r) {
  coefs_r <- coef(r$model)
  tibble(
    quintile     = r$quintile,
    breakpoint   = r$breakpoint,
    slope_below  = coefs_r["HI_below_r"],
    slope_above  = coefs_r["HI_above_r"],
    coef_precip  = coefs_r["Precipitation"],
    coef_precip2 = coefs_r[grep("Precipitation\\^2", names(coefs_r), value = TRUE)],
    coef_wind    = coefs_r["Wind_Speed"]
  )
})
saveRDS(plot_coefs_separate_r, file.path(out_dir_r, "rebate_plot_coefs_separate.rds"))

rm(results_separate_r)
gc(); gc()

### ============================================================
###   VERSION 2: POOLED MODEL (1 breakpoint, quintile interactions)
### ============================================================

set.seed(42)
df_sample_r  <- rebate_with_quint_r %>% slice_sample(prop = 1)
lm_pooled_r  <- lm(log_consumption ~ HI_F_new_r, data = df_sample_r)
seg_pooled_r <- segmented(lm_pooled_r, seg.Z = ~HI_F_new_r, psi = 100)
bp_pooled_r  <- seg_pooled_r$psi[2]

cat("\n=== POOLED BREAKPOINT:", round(bp_pooled_r, 2), "°F ===\n")
davies_pooled_r <- davies.test(lm_pooled_r, seg.Z = ~HI_F_new_r)
cat("Davies p-value:", davies_pooled_r$p.value, "\n")

rm(lm_pooled_r, seg_pooled_r, df_sample_r); gc()

rebate_with_quint_r$HI_below_r <- pmin(rebate_with_quint_r$HI_F_new_r, bp_pooled_r)
rebate_with_quint_r$HI_above_r <- pmax(0, rebate_with_quint_r$HI_F_new_r - bp_pooled_r)
gc()

fe_pooled_r <- feols(
  log_consumption ~ HI_below_r:income_quintile + HI_above_r:income_quintile +
    Precipitation + I(Precipitation^2) + Wind_Speed |
    PREM_ID + MONTH,
  cluster = ~PREM_ID, data = rebate_with_quint_r
)
summary(fe_pooled_r)

### ============================================================
###   VERSION 2: POOLED MODEL (1 breakpoint, quintile interactions)
### ============================================================

# Sample to estimate breakpoint — avoids memory crash on full dataset
set.seed(42)
df_sample_r  <- df_r %>% slice_sample(prop = .5)
lm_pooled_r  <- lm(log_consumption ~ HI_F_new_r, data = df_sample_r)
seg_pooled_r <- segmented(lm_pooled_r, seg.Z = ~HI_F_new_r, psi = 100)
bp_pooled_r  <- seg_pooled_r$psi[2]

cat("\n=== POOLED BREAKPOINT:", round(bp_pooled, 4), "°F ===\n")
davies_pooled_r <- davies.test(lm_pooled_r, seg.Z = ~HI_F_new_r)
cat("Davies p-value:", davies_pooled_r$p.value, "\n")

rm(lm_pooled_r, seg_pooled_r, df_sample_r); gc()

# Mutate df in place — avoids creating a full copy
df$HI_below_r <- pmin(df_r$HI_F_new_r, bp_pooled_r)
df$HI_above_r <- pmax(0, df_r$HI_F_new_r - bp_pooled_r)

# Drop redundant columns
df$HI_F_new_r <- NULL
df$HI_F_New_r <- NULL
gc()

fe_pooled_r <- feols(
  log_consumption ~ HI_below:income_quintile + HI_above:income_quintile +
    Precipitation + I(Precipitation^2) + Wind_Speed |
    PREM_ID + MONTH,
  cluster = ~PREM_ID, data = df_r
)
summary(fe_pooled_r)

### ============================================================
###   WALD TESTS
### ============================================================

b_r  <- coef(fe_pooled_r); V_r <- vcov(fe_pooled_r); cn_r <- names(b_r)

coefs_above_r <- paste0("income_quintile", c("Q1: 0-20%","Q2: 20-40%","Q3: 40-60%","Q4: 60-80%","Q5: 80-100%"), ":HI_above_r")
coefs_below_r <- paste0("HI_below_r:income_quintile", c("Q1: 0-20%","Q2: 20-40%","Q3: 40-60%","Q4: 60-80%","Q5: 80-100%"))

cat("Above coefs found:", all(coefs_above_r %in% cn_r), "\n")
cat("Below coefs found:", all(coefs_below_r %in% cn_r), "\n")

L_above_r <- t(sapply(coefs_above_r[1:4], function(x) as.numeric(cn_r == x) - as.numeric(cn_r == coefs_above_r[5])))
L_below_r <- t(sapply(coefs_below_r[1:4], function(x) as.numeric(cn_r == x) - as.numeric(cn_r == coefs_below_r[5])))

cat("\n=== WALD TEST: ABOVE BREAKPOINT ===\n"); print(wald.test(b = b_r, Sigma = V_r, L = L_above_r))
cat("\n=== WALD TEST: BELOW BREAKPOINT ===\n"); print(wald.test(b = b_r, Sigma = V_r, L = L_below_r))

### ============================================================
###   LIKELIHOOD RATIO TEST
### ============================================================

loglik_separate_r <- readRDS(file.path(out_dir_r, "rebate_loglik_separate.rds"))

LL_separate_r <- sum(loglik_separate_r)
LL_pooled_r   <- logLik(fe_pooled_r)[1]
LR_stat_r     <- 2 * (LL_separate_r - LL_pooled_r)
lr_df_r       <- 4
p_value_r     <- pchisq(LR_stat_r, df = lr_df_r, lower.tail = FALSE)

cat("\n================================================================\n")
cat("   LIKELIHOOD RATIO TEST\n")
cat("================================================================\n")
cat(sprintf("LL (separate): %10.2f\n", LL_separate_r))
cat(sprintf("LL (pooled):   %10.2f\n", LL_pooled_r))
cat(sprintf("LR statistic:  %10.2f\n", LR_stat_r))
cat(sprintf("df:            %10d\n",   lr_df_r))
cat(sprintf("P-value:       %10.4f%s\n", p_value_r,
            ifelse(p_value_r < 0.001, " ***",
                   ifelse(p_value_r < 0.01,  " **",
                          ifelse(p_value_r < 0.05,  " *", "")))))

FINAL_MODEL_r <- ifelse(p_value_r < 0.05, "SEPARATE", "POOLED")
cat("\nFINAL MODEL:", FINAL_MODEL_r, "\n================================================================\n\n")

### ============================================================
###   PLOTS
### ============================================================

line_types_r <- c("solid", "dashed", "dotdash", "dotted", "twodash")
nature_palette_r <- c(
  "Q1: 0-20%"   = "#2A9D8F",
  "Q2: 20-40%"  = "#4C67A1",
  "Q3: 40-60%"  = "#9055A2",
  "Q4: 60-80%"  = "#E9C46A",
  "Q5: 80-100%" = "#D37256"
)

quintile_levels_r <- c("Q1: 0-20%", "Q2: 20-40%", "Q3: 40-60%", "Q4: 60-80%", "Q5: 80-100%")

# --- Pooled plot ---
pred_grid_r <- expand.grid(
  HI_F_new_r      = seq(90, 115, by = 0.5),
  income_quintile = quintile_levels_r
) %>%
  mutate(
    income_quintile = factor(income_quintile, levels = quintile_levels_r),
    HI_below_r      = pmin(HI_F_new_r, bp_pooled_r),
    HI_above_r      = pmax(0, HI_F_new_r - bp_pooled_r),
    Precipitation   = mean(rebate_with_quint_r$Precipitation, na.rm = TRUE),
    Wind_Speed      = mean(rebate_with_quint_r$Wind_Speed,    na.rm = TRUE),
    PREM_ID         = rebate_with_quint_r$PREM_ID[1],
    MONTH           = rebate_with_quint_r$MONTH[1]
  )

pred_grid_r$fit_r <- predict(fe_pooled_r, newdata = pred_grid_r)

pred_grid_r <- pred_grid_r %>%
  group_by(income_quintile) %>%
  mutate(
    actual_mean_r = mean(rebate_with_quint_r$log_consumption[rebate_with_quint_r$income_quintile == first(income_quintile)], na.rm = TRUE),
    fit_aligned_r = fit_r + (actual_mean_r - mean(fit_r, na.rm = TRUE))
  ) %>% ungroup()

bp_points_r <- pred_grid_r %>%
  group_by(income_quintile) %>%
  slice(which.min(abs(HI_F_new_r - bp_pooled_r))) %>% ungroup()

ggplot(pred_grid_r, aes(x = HI_F_new_r, y = fit_aligned_r, group = income_quintile)) +
  geom_line(aes(linetype = income_quintile), color = "black", linewidth = 1) +
  geom_vline(xintercept = bp_pooled_r, linetype = "dotted", linewidth = 0.6) +
  geom_point(data = bp_points_r, aes(fill = income_quintile),
             shape = 21, size = 5, stroke = 0.7, alpha = 0.66, color = "black") +
  annotate("text", x = bp_pooled_r + 1, y = max(pred_grid_r$fit_aligned_r) - 0.05,
           label = paste0("BP: ", round(bp_pooled_r, 2), "°F"), hjust = 0, size = 4) +
  scale_linetype_manual(values = line_types_r, name = "Income Quintile") +
  scale_fill_manual(values = nature_palette_r, name = "Income Quintile") +
  guides(linetype = guide_legend(override.aes = list(linewidth = 1.2), keywidth = unit(1, "cm")),
         fill     = guide_legend(override.aes = list(size = 3))) +
  labs(x = "Heat Index (°F)", y = "Log of Daily Electricity Consumption") +
  coord_cartesian(xlim = c(90, 115), ylim = c(3, 4.1)) +
  theme_nature_r()

ggsave(file.path(out_dir_r, "rebate_income_quintile_pooled.png"), width = 10, height = 6, dpi = 300)

# --- Separate models plot ---
plot_coefs_separate_r <- readRDS(file.path(out_dir_r, "rebate_plot_coefs_separate.rds"))

predict_separate_r <- function(row_r, hi_seq_r) {
  coef_p2_r <- if (is.na(row_r$coef_precip2)) 0 else row_r$coef_precip2
  tibble(
    HI_F_new_r      = hi_seq_r,
    income_quintile = row_r$quintile,
    HI_below_r      = pmin(hi_seq_r, row_r$breakpoint),
    HI_above_r      = pmax(0, hi_seq_r - row_r$breakpoint),
    breakpoint      = row_r$breakpoint
  ) %>%
    mutate(
      fit_r = row_r$slope_below * HI_below_r +
        row_r$slope_above  * HI_above_r +
        row_r$coef_precip  * mean(rebate_with_quint_r$Precipitation,   na.rm = TRUE) +
        coef_p2_r          * mean(rebate_with_quint_r$Precipitation^2, na.rm = TRUE) +
        row_r$coef_wind    * mean(rebate_with_quint_r$Wind_Speed,      na.rm = TRUE)
    )
}

actual_means_vec_r <- rebate_with_quint_r %>%
  group_by(income_quintile) %>%
  summarise(m_r = mean(log_consumption, na.rm = TRUE), .groups = "drop") %>%
  mutate(income_quintile = as.character(income_quintile)) %>%
  { setNames(.$m_r, .$income_quintile) }

pred_separate_r <- map_df(
  split(plot_coefs_separate_r, plot_coefs_separate_r$quintile),
  ~predict_separate_r(.x, seq(90, 115, by = 0.5))
) %>%
  mutate(
    actual_mean_r   = actual_means_vec_r[income_quintile],
    income_quintile = factor(income_quintile, levels = levels(rebate_with_quint_r$income_quintile))
  ) %>%
  group_by(income_quintile) %>%
  mutate(fit_aligned_r = fit_r + (actual_mean_r - mean(fit_r, na.rm = TRUE))) %>%
  ungroup()

bp_points_separate_r <- pred_separate_r %>%
  group_by(income_quintile) %>%
  slice(which.min(abs(HI_F_new_r - breakpoint))) %>% ungroup()

ggplot(pred_separate_r, aes(x = HI_F_new_r, y = fit_aligned_r, group = income_quintile)) +
  geom_line(aes(linetype = income_quintile), color = "black", linewidth = 1) +
  geom_point(data = bp_points_separate_r, aes(fill = income_quintile),
             shape = 21, size = 6, stroke = 0.5, alpha = 0.7, color = "black") +
  scale_linetype_manual(values = line_types_r, name = "Income Quintile") +
  scale_fill_manual(values = nature_palette_r, name = "Income Quintile") +
  scale_x_continuous(breaks = seq(90, 115, by = 5)) +
  scale_y_continuous(breaks = seq(3, 4.1, by = 0.2)) +
  guides(linetype = guide_legend(override.aes = list(linewidth = 1), keywidth = unit(1.5, "cm")),
         fill     = guide_legend(override.aes = list(size = 4))) +
  labs(x = "Heat Index (°F)", y = "Log of Daily Electricity Consumption") +
  coord_cartesian(xlim = c(90, 115), ylim = c(3, 4.1)) +
  theme_nature_r()

ggsave(file.path(out_dir_r, "rebate_income_quintile_separate.png"), width = 10, height = 6, dpi = 300)

### ============================================================
###   SUMMARY TABLES + FINAL RECOMMENDATION
### ============================================================

separate_summary_r <- read_csv(file.path(out_dir_r, "rebate_separate_summary.csv"))


pooled_coef_summary_r <- tibble(
  income_quintile = c("Q1: 0-20%", "Q2: 20-40%", "Q3: 40-60%", "Q4: 60-80%", "Q5: 80-100%"),
  slope_below = round(coef(fe_pooled_r)[coefs_below_r], 4),
  se_below    = round(sqrt(diag(vcov(fe_pooled_r)))[coefs_below_r], 4),
  slope_above = round(coef(fe_pooled_r)[coefs_above_r], 4),
  se_above    = round(sqrt(diag(vcov(fe_pooled_r)))[coefs_above_r], 4)
)

cat("\n=== POOLED MODEL COEFFICIENTS ===\n"); print(pooled_coef_summary_r)
cat("\nSeparate breakpoints:", paste(round(separate_summary_r$breakpoint, 5), collapse = ", "), "°F\n")
cat("Pooled breakpoint:", round(bp_pooled_r, 5), "°F\n")

if (FINAL_MODEL_r == "SEPARATE") {
  cat("\n✓ USE: SEPARATE MODELS\n")
  print(separate_summary_r)
  
  cat("\nSLOPE COMPARISON Z-TESTS (each quintile vs Q5):\n\n")
  for (i in 1:4) {
    z_above_r <- (separate_summary_r$slope_above[i] - separate_summary_r$slope_above[5]) /
      sqrt(separate_summary_r$se_above[i]^2 + separate_summary_r$se_above[5]^2)
    z_below_r <- (separate_summary_r$slope_below[i] - separate_summary_r$slope_below[5]) /
      sqrt(separate_summary_r$se_below[i]^2 + separate_summary_r$se_below[5]^2)
    p_above_r <- 2 * pnorm(-abs(z_above_r))
    p_below_r <- 2 * pnorm(-abs(z_below_r))
    cat(sprintf("%s vs Q5: Above BP: z = %6.2f, p = %.4f%s | Below BP: z = %6.2f, p = %.4f%s\n",
                separate_summary_r$income_quintile[i],
                z_above_r, p_above_r, ifelse(p_above_r < 0.05, " ***", ""),
                z_below_r, p_below_r, ifelse(p_below_r < 0.05, " ***", "")))
  }
} else {
  cat("\n✓ USE: POOLED MODEL — breakpoint =", round(bp_pooled_r, 2), "°F\n")
  print(pooled_coef_summary_r)
  cat("\nWald test results: see output above\n")
}

income_summary_r <- rebate_with_quint_r %>%
  distinct(PREM_ID, income_quintile, median_hh_income) %>%
  group_by(income_quintile) %>%
  summarise(
    avg_median_income = mean(median_hh_income, na.rm = TRUE),
    median_income     = median(median_hh_income, na.rm = TRUE),
    min_income        = min(median_hh_income, na.rm = TRUE),
    max_income        = max(median_hh_income, na.rm = TRUE),
    n_households      = n()
  )

print(income_summary_r)

