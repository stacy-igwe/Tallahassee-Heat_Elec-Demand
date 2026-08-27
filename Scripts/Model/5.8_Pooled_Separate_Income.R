### ============================================================
###   SEGMENTED FE BY INCOME QUINTILE
### ============================================================
library(tidyverse)
library(fixest)
library(segmented)
library(lubridate)
library(scales)
library(aod)
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

### --- 1. Load data ---
df <- read_csv(
  "~/Library/Mobile Documents/com~apple~CloudDocs/April Complete Reset/Data/Data-Output/Legit Data Output/5.7_clean_merged_weather_quintiles_2019.csv"
) %>%
  filter(Daily_Consumption > 0) %>%
  mutate(
    DATE            = ymd(DATE),
    HI_F_new        = HI_F_New,
    log_consumption = log(Daily_Consumption),
    MONTH           = factor(lubridate::month(DATE, label = TRUE, abbr = TRUE),
                             levels = c("May","Jun","Jul","Aug","Sep")),
    income_quintile = factor(
      income_quintile,
      levels = c("Q1: 0-20%", "Q2: 20-40%", "Q3: 40-60%", "Q4: 60-80%", "Q5: 80-100%")
    )
  )

cat("=== FULL DATASET BY INCOME QUINTILE ===\n")
df %>%
  group_by(income_quintile) %>%
  summarise(n_obs = n(), n_hh = n_distinct(PREM_ID)) %>%
  print()
cat("\nTotal households:", n_distinct(df$PREM_ID), "\n")

### ============================================================
###   VERSION 1: SEPARATE MODELS (5 models, 5 breakpoints)
### ============================================================

run_separate_model <- function(df_subset, group_name) {
  lm_fit  <- lm(log_consumption ~ HI_F_new, data = df_subset)
  seg_fit <- tryCatch(
    segmented(lm_fit, seg.Z = ~HI_F_new, psi = 100),
    error = function(e) NULL
  )
  bp  <- if (!is.null(seg_fit)) seg_fit$psi[2] else 104
  dav <- tryCatch(davies.test(lm_fit, seg.Z = ~HI_F_new),
                  error = function(e) list(p.value = NA))
  
  df_subset <- df_subset %>%
    mutate(HI_below = pmin(HI_F_new, bp), HI_above = pmax(0, HI_F_new - bp))
  
  fe_fit <- feols(
    log_consumption ~ HI_below + HI_above +
      Precipitation + I(Precipitation^2) + Wind_Speed |
      PREM_ID + MONTH,
    cluster = ~PREM_ID, data = df_subset
  )
  
  list(quintile = group_name, breakpoint = bp, davies_p = dav$p.value,
       model = fe_fit, n_hh = n_distinct(df_subset$PREM_ID), n_obs = nrow(df_subset))
}

results_separate <- df %>%
  group_split(income_quintile) %>%
  map(~run_separate_model(.x, as.character(unique(.x$income_quintile))))

separate_summary <- map_df(results_separate, function(r) {
  coefs <- coef(r$model); ses <- sqrt(diag(vcov(r$model)))
  tibble(
    income_quintile = r$quintile, n_hh = r$n_hh,
    breakpoint  = round(r$breakpoint, 2), davies_p = r$davies_p,
    slope_below = round(coefs["HI_below"], 4), se_below = round(ses["HI_below"], 4),
    slope_above = round(coefs["HI_above"], 4), se_above = round(ses["HI_above"], 4)
  )
})

# Save separate models output to file
out_dir <- "~/Library/Mobile Documents/com~apple~CloudDocs/April Complete Reset/Data/Data-Output"
sink(file.path(out_dir, "separate_models_results.txt"))
cat("\n=== SEPARATE MODELS SUMMARY ===\n")
print(separate_summary)
for (r in results_separate) {
  cat("\n===", r$quintile, "===\n")
  cat("Breakpoint:", round(r$breakpoint, 2), "°F | Households:", r$n_hh, "\n")
  print(summary(r$model))
}
sink()

gc()

### ============================================================
###   AFTER SEPARATE MODELS — SAVE & PURGE BEFORE POOLED
### ============================================================

out_dir <- "~/Library/Mobile Documents/com~apple~CloudDocs/April Complete Reset/Data/Data-Output"

# 1. Save separate_summary as CSV (lightweight, easy to reload)
write_csv(separate_summary, file.path(out_dir, "separate_summary.csv"))

# 2. Save the full results_separate list as RDS (so you can reload log-likelihoods later)
saveRDS(results_separate, file.path(out_dir, "results_separate.rds"))

# 3. Extract ONLY what you need downstream before nuking the list
loglik_separate <- map_dbl(results_separate, ~logLik(.x$model)[1])
saveRDS(loglik_separate, file.path(out_dir, "loglik_separate.rds"))

# 4. Nuke the model list — this is your biggest memory hog
rm(results_separate)
gc(); gc()  # double gc() helps R actually release to OS on Mac

### ============================================================
###   VERSION 2: POOLED MODEL — MEMORY-LEAN VERSION
### ============================================================

# Sample to estimate breakpoint
set.seed(42)
df_sample  <- df %>% slice_sample(prop = .5)
lm_pooled  <- lm(log_consumption ~ HI_F_new, data = df_sample)
seg_pooled <- segmented(lm_pooled, seg.Z = ~HI_F_new, psi = 100)
bp_pooled  <- seg_pooled$psi[2]

cat("\n=== POOLED BREAKPOINT:", round(bp_pooled, 2), "°F ===\n")
davies_pooled <- davies.test(lm_pooled, seg.Z = ~HI_F_new)
cat("Davies p-value:", davies_pooled$p.value, "\n")

rm(lm_pooled, seg_pooled, df_sample); gc()

# 5. AVOID creating df_pooled as a full copy — mutate df in place
#    Base R assignment does NOT trigger a full copy the way dplyr pipes do
df$HI_below <- pmin(df$HI_F_new, bp_pooled)
df$HI_above <- pmax(0, df$HI_F_new - bp_pooled)

# 6. Drop any columns you no longer need before feols
#    (HI_F_new is now redundant; keep only what feols needs)
df$HI_F_new <- NULL
df$HI_F_New <- NULL   # original column if it still exists
gc()

fe_pooled <- feols(
  log_consumption ~ HI_below:income_quintile + HI_above:income_quintile +
    Precipitation + I(Precipitation^2) + Wind_Speed |
    PREM_ID + MONTH,
  cluster = ~PREM_ID, data = df
)

### ============================================================
###   VERSION 2: POOLED MODEL (1 breakpoint, quintile interactions)
### ============================================================

# Sample to estimate breakpoint — avoids memory crash on full dataset
set.seed(42)
df_sample  <- df %>% slice_sample(prop = .1)
lm_pooled  <- lm(log_consumption ~ HI_F_new, data = df_sample)
seg_pooled <- segmented(lm_pooled, seg.Z = ~HI_F_new, psi = 100)
bp_pooled  <- seg_pooled$psi[2]

cat("\n=== POOLED BREAKPOINT:", round(bp_pooled, 2), "°F ===\n")
davies_pooled <- davies.test(lm_pooled, seg.Z = ~HI_F_new)
cat("Davies p-value:", davies_pooled$p.value, "\n")

rm(lm_pooled, seg_pooled, df_sample); gc()

df_pooled <- df %>%
  mutate(
    HI_below = pmin(HI_F_new, bp_pooled),
    HI_above = pmax(0, HI_F_new - bp_pooled)
  )

fe_pooled <- feols(
  log_consumption ~ HI_below:income_quintile + HI_above:income_quintile +
    Precipitation + I(Precipitation^2) + Wind_Speed |
    PREM_ID + MONTH,
  cluster = ~PREM_ID, data = df_pooled
)
summary(fe_pooled)

### ============================================================
###   WALD TESTS
### ============================================================

b  <- coef(fe_pooled); V <- vcov(fe_pooled); cn <- names(b)

coefs_above <- paste0("income_quintile", c("Q1: 0-20%","Q2: 20-40%","Q3: 40-60%","Q4: 60-80%","Q5: 80-100%"), ":HI_above")
coefs_below <- paste0("HI_below:income_quintile", c("Q1: 0-20%","Q2: 20-40%","Q3: 40-60%","Q4: 60-80%","Q5: 80-100%"))

cat("Above coefs found:", all(coefs_above %in% cn), "\n")
cat("Below coefs found:", all(coefs_below %in% cn), "\n")

L_above <- t(sapply(coefs_above[1:4], function(x) as.numeric(cn == x) - as.numeric(cn == coefs_above[5])))
L_below <- t(sapply(coefs_below[1:4], function(x) as.numeric(cn == x) - as.numeric(cn == coefs_below[5])))

cat("\n=== WALD TEST: ABOVE BREAKPOINT ===\n"); print(wald.test(b = b, Sigma = V, L = L_above))
cat("\n=== WALD TEST: BELOW BREAKPOINT ===\n"); print(wald.test(b = b, Sigma = V, L = L_below))

loglik_separate <- readRDS(file.path(out_dir, "loglik_separate.rds"))
### ============================================================
###   LIKELIHOOD RATIO TEST
### ============================================================

# loglik_separate is already loaded from RDS above — DELETE the line below:
# loglik_separate <- map_dbl(results_separate, ~logLik(.x$model)[1])  ← REMOVE THIS

LL_separate     <- sum(loglik_separate)
LL_pooled       <- logLik(fe_pooled)[1]
LR_stat         <- 2 * (LL_separate - LL_pooled)
lr_df           <- 4
p_value         <- pchisq(LR_stat, df = lr_df, lower.tail = FALSE)

cat("\n================================================================\n")
cat("   LIKELIHOOD RATIO TEST\n")
cat("================================================================\n")
cat(sprintf("LL (separate): %10.2f\n", LL_separate))
cat(sprintf("LL (pooled):   %10.2f\n", LL_pooled))
cat(sprintf("LR statistic:  %10.2f\n", LR_stat))
cat(sprintf("df:            %10d\n",   lr_df))
cat(sprintf("P-value:       %10.4f%s\n", p_value,
            ifelse(p_value < 0.001, " ***",
                   ifelse(p_value < 0.01,  " **",
                          ifelse(p_value < 0.05,  " *", "")))))

FINAL_MODEL <- ifelse(p_value < 0.05, "SEPARATE", "POOLED")
cat("\nFINAL MODEL:", FINAL_MODEL, "\n================================================================\n\n")
### ============================================================
###   PLOTS
### ============================================================

line_types <- c("solid", "dashed", "dotdash", "dotted", "twodash")
nature_palette <- c(
  "Q1: 0-20%"  = "#2A9D8F",
  "Q2: 20-40%" = "#4C67A1",
  "Q3: 40-60%" = "#9055A2",
  "Q4: 60-80%" = "#E9C46A",
  "Q5: 80-100%" = "#D37256"
)

# --- Pooled plot: replace df_pooled with df everywhere ---
pred_grid <- expand.grid(
  HI_F_new = seq(80, 115, by = 0.5),
  income_quintile = levels(df$income_quintile)          # df not df_pooled
) %>%
  mutate(
    income_quintile = factor(income_quintile, levels = levels(df$income_quintile)),
    HI_below      = pmin(HI_F_new, bp_pooled),
    HI_above      = pmax(0, HI_F_new - bp_pooled),
    Precipitation = mean(df$Precipitation, na.rm = TRUE),  # df not df_pooled
    Wind_Speed    = mean(df$Wind_Speed,    na.rm = TRUE),  # df not df_pooled
    PREM_ID       = df$PREM_ID[1],                         # df not df_pooled
    MONTH         = df$MONTH[1]                            # df not df_pooled
  )

pred_grid$fit <- predict(fe_pooled, newdata = pred_grid)

pred_grid <- pred_grid %>%
  group_by(income_quintile) %>%
  mutate(
    actual_mean = mean(df$log_consumption[df$income_quintile == first(income_quintile)], na.rm = TRUE),  # df
    fit_aligned = fit + (actual_mean - mean(fit, na.rm = TRUE))
  ) %>% ungroup()

bp_points <- pred_grid %>%
  group_by(income_quintile) %>%
  slice(which.min(abs(HI_F_new - bp_pooled))) %>% ungroup()

ggplot(pred_grid, aes(x = HI_F_new, y = fit_aligned, group = income_quintile)) +
  geom_line(aes(linetype = income_quintile), color = "black", linewidth = 1) +
  geom_vline(xintercept = bp_pooled, linetype = "dotted", linewidth = 0.6) +
  geom_point(data = bp_points, aes(fill = income_quintile),
             shape = 21, size = 5, stroke = 0.7, alpha = 0.66, color = "black") +
  annotate("text", x = bp_pooled + 1, y = max(pred_grid$fit_aligned) - 0.05,
           label = paste0("BP: ", round(bp_pooled, 2), "°F"), hjust = 0, size = 4) +
  scale_linetype_manual(values = line_types, name = "Income Quintile") +
  scale_fill_manual(values = nature_palette, name = "Income Quintile") +
  guides(linetype = guide_legend(override.aes = list(linewidth = 1.2), keywidth = unit(1, "cm")),
         fill     = guide_legend(override.aes = list(size = 3))) +
  labs(x = "Heat Index (°F)", y = "Log of Daily Electricity Consumption") +
  coord_cartesian(xlim = c(85, 115), ylim = c(3, 4.1)) +
  theme_nature()

ggsave("full_income_quintile_pooled.png", width = 10, height = 6, dpi = 300)

# --- Separate models plot ---
predict_separate <- function(result, hi_seq) {
  bp <- result$breakpoint; coefs <- coef(result$model)
  tibble(HI_F_new = hi_seq, income_quintile = result$quintile,
         HI_below = pmin(HI_F_new, bp), HI_above = pmax(0, HI_F_new - bp),
         breakpoint = bp) %>%
    mutate(
      fit = coefs["HI_below"] * HI_below + coefs["HI_above"] * HI_above +
        coefs["Precipitation"]      * mean(df$Precipitation,    na.rm = TRUE) +
        coefs["I(Precipitation^2)"] * mean(df$Precipitation^2, na.rm = TRUE) +
        coefs["Wind_Speed"]         * mean(df$Wind_Speed,       na.rm = TRUE)
    )
}

results_separate <- readRDS(file.path(out_dir, "results_separate.rds"))

pred_separate <- map_df(results_separate, ~predict_separate(.x, seq(88, 115, by = 0.5))) %>%
  mutate(income_quintile = factor(income_quintile, levels = levels(df$income_quintile))) %>%
  group_by(income_quintile) %>%
  mutate(
    actual_mean = mean(df$log_consumption[df$income_quintile == first(income_quintile)], na.rm = TRUE),
    fit_aligned = fit + (actual_mean - mean(fit, na.rm = TRUE))
  ) %>% ungroup()

bp_points_separate <- pred_separate %>%
  group_by(income_quintile) %>%
  slice(which.min(abs(HI_F_new - breakpoint))) %>% ungroup()

ggplot(pred_separate, aes(x = HI_F_new, y = fit_aligned, group = income_quintile)) +
  geom_line(aes(linetype = income_quintile), color = "black", linewidth = 1) +
  geom_point(data = bp_points_separate, aes(fill = income_quintile),
             shape = 21, size = 6, stroke = 0.5, alpha = 0.7, color = "black") +
  scale_linetype_manual(values = line_types, name = "Income Quintile") +
  scale_fill_manual(values = nature_palette, name = "Income Quintile") +
  scale_x_continuous(breaks = seq(88, 115, by = 5)) +
  scale_y_continuous(breaks = seq(3, 4.1, by = 0.2)) +
  guides(linetype = guide_legend(override.aes = list(linewidth = 1), keywidth = unit(1.5, "cm")),
         fill     = guide_legend(override.aes = list(size = 4))) +
  labs(x = "Heat Index (°F)", y = "Log of Daily Electricity Consumption (kWh)") +
  coord_cartesian(xlim = c(88, 115), ylim = c(3, 4.1)) +
  theme_nature()

ggsave("full_income_quintile_separate.png", width = 10, height = 6, dpi = 300)
rm(results_separate); gc()
separate_summary <- read_csv(file.path(out_dir, "separate_summary.csv"))
### ============================================================
###   SUMMARY TABLES + FINAL RECOMMENDATION
### ============================================================

pooled_coef_summary <- tibble(
  income_quintile = c("Q1: 0-20%","Q2: 20-40%","Q3: 40-60%","Q4: 60-80%","Q5: 80-100%"),
  slope_below = round(coef(fe_pooled)[coefs_below], 4),
  se_below    = round(sqrt(diag(vcov(fe_pooled)))[coefs_below], 4),
  slope_above = round(coef(fe_pooled)[coefs_above], 4),
  se_above    = round(sqrt(diag(vcov(fe_pooled)))[coefs_above], 4)
)

cat("\n=== POOLED MODEL COEFFICIENTS ===\n"); print(pooled_coef_summary)
cat("\nSeparate breakpoints:", paste(round(separate_summary$breakpoint, 1), collapse = ", "), "°F\n")
cat("Pooled breakpoint:", round(bp_pooled, 1), "°F\n")

if (FINAL_MODEL == "SEPARATE") {
  cat("\n✓ USE: SEPARATE MODELS\n")
  print(separate_summary)
  
  cat("\nSLOPE COMPARISON Z-TESTS (each quintile vs Q5):\n\n")
  for (i in 1:4) {
    z_above <- (separate_summary$slope_above[i] - separate_summary$slope_above[5]) /
      sqrt(separate_summary$se_above[i]^2 + separate_summary$se_above[5]^2)
    z_below <- (separate_summary$slope_below[i] - separate_summary$slope_below[5]) /
      sqrt(separate_summary$se_below[i]^2 + separate_summary$se_below[5]^2)
    p_above <- 2 * pnorm(-abs(z_above))
    p_below <- 2 * pnorm(-abs(z_below))
    cat(sprintf("%s vs Q5: Above BP: z = %6.2f, p = %.4f%s | Below BP: z = %6.2f, p = %.4f%s\n",
                separate_summary$income_quintile[i],
                z_above, p_above, ifelse(p_above < 0.05, " ***", ""),
                z_below, p_below, ifelse(p_below < 0.05, " ***", "")))
  }
} else {
  cat("\n✓ USE: POOLED MODEL — breakpoint =", round(bp_pooled, 2), "°F\n")
  print(pooled_coef_summary)
  cat("\nWald test results: see output above\n")
}