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
df <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/April Complete Reset/Data/Data-Output/Legit Data Output/5.12_HI_clean_merged_weather_quintiles_2019_CORRECT.csv")%>%
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

names(df)

#Number of households is 89,630

cat("=== FULL DATASET BY INCOME QUINTILE ===\n")
df %>%
  group_by(income_quintile) %>%
  summarise(n_obs = n(), n_hh = n_distinct(PREM_ID)) %>%
  print()
cat("\nTotal households:", n_distinct(df$PREM_ID), "\n")

out_dir <- "~/Library/Mobile Documents/com~apple~CloudDocs/April Complete Reset/Images"

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
    breakpoint  = round(r$breakpoint, 4), davies_p = r$davies_p,
    slope_below = round(coefs["HI_below"], 4), se_below = round(ses["HI_below"], 4),
    slope_above = round(coefs["HI_above"], 4), se_above = round(ses["HI_above"], 4)
  )
})

# Save separate models output to file
sink(file.path(out_dir, "separate_models_results.txt"))
cat("\n=== SEPARATE MODELS SUMMARY ===\n")
print(separate_summary)
for (r in results_separate) {
  cat("\n===", r$quintile, "===\n")
  cat("Breakpoint:", round(r$breakpoint, 4), "°F | Households:", r$n_hh, "\n")
  print(summary(r$model))
}
sink()

### ============================================================
###   SAVE & PURGE BEFORE POOLED MODEL
### ============================================================

# Save separate_summary as CSV
write_csv(separate_summary, file.path(out_dir, "separate_summary.csv"))

# Save log-likelihoods for LR test
loglik_separate <- map_dbl(results_separate, ~logLik(.x$model)[1])
saveRDS(loglik_separate, file.path(out_dir, "loglik_separate.rds"))

# Save lightweight coefs for the separate plot — avoids reloading full model objects later
plot_coefs_separate <- map_df(results_separate, function(r) {
  coefs <- coef(r$model)
  tibble(
    quintile     = r$quintile,
    breakpoint   = r$breakpoint,
    slope_below  = coefs["HI_below"],
    slope_above  = coefs["HI_above"],
    coef_precip  = coefs["Precipitation"],
    coef_precip2 = coefs[grep("Precipitation\\^2", names(coefs), value = TRUE)],
    coef_wind    = coefs["Wind_Speed"]
  )
})
saveRDS(plot_coefs_separate, file.path(out_dir, "plot_coefs_separate.rds"))

# Nuke the model list
rm(results_separate)
gc(); gc()

### ============================================================
###   VERSION 2: POOLED MODEL (1 breakpoint, quintile interactions)
### ============================================================

# Sample to estimate breakpoint — avoids memory crash on full dataset
set.seed(42)
df_sample  <- df %>% slice_sample(prop = 1)
lm_pooled  <- lm(log_consumption ~ HI_F_new, data = df_sample)
seg_pooled <- segmented(lm_pooled, seg.Z = ~HI_F_new, psi = 100)
bp_pooled  <- seg_pooled$psi[2]

cat("\n=== POOLED BREAKPOINT:", round(bp_pooled, 4), "°F ===\n")
davies_pooled <- davies.test(lm_pooled, seg.Z = ~HI_F_new)
cat("Davies p-value:", davies_pooled$p.value, "\n")

rm(lm_pooled, seg_pooled, df_sample); gc()

# Mutate df in place — avoids creating a full copy
df$HI_below <- pmin(df$HI_F_new, bp_pooled)
df$HI_above <- pmax(0, df$HI_F_new - bp_pooled)

# Drop redundant columns
df$HI_F_new <- NULL
df$HI_F_New <- NULL
gc()

fe_pooled <- feols(
  log_consumption ~ HI_below:income_quintile + HI_above:income_quintile +
    Precipitation + I(Precipitation^2) + Wind_Speed |
    PREM_ID + MONTH,
  cluster = ~PREM_ID, data = df
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

### ============================================================
###   LIKELIHOOD RATIO TEST
### ============================================================

loglik_separate <- readRDS(file.path(out_dir, "loglik_separate.rds"))

LL_separate <- sum(loglik_separate)
LL_pooled   <- logLik(fe_pooled)[1]
LR_stat     <- 2 * (LL_separate - LL_pooled)
lr_df       <- 4
p_value     <- pchisq(LR_stat, df = lr_df, lower.tail = FALSE)

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
  "Q1: 0-20%"   = "#2A9D8F",
  "Q2: 20-40%"  = "#4C67A1",
  "Q3: 40-60%"  = "#9055A2",
  "Q4: 60-80%"  = "#E9C46A",
  "Q5: 80-100%" = "#D37256"
)

# --- Pooled plot ---
pred_grid <- expand.grid(
  HI_F_new        = seq(80, 115, by = 0.5),
  income_quintile = levels(df$income_quintile)
) %>%
  mutate(
    income_quintile = factor(income_quintile, levels = levels(df$income_quintile)),
    HI_below        = pmin(HI_F_new, bp_pooled),
    HI_above        = pmax(0, HI_F_new - bp_pooled),
    Precipitation   = mean(df$Precipitation, na.rm = TRUE),
    Wind_Speed      = mean(df$Wind_Speed,    na.rm = TRUE),
    PREM_ID         = df$PREM_ID[1],
    MONTH           = df$MONTH[1]
  )

pred_grid$fit <- predict(fe_pooled, newdata = pred_grid)

pred_grid <- pred_grid %>%
  group_by(income_quintile) %>%
  mutate(
    actual_mean = mean(df$log_consumption[df$income_quintile == first(income_quintile)], na.rm = TRUE),
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
  coord_cartesian(xlim = c(90, 115), ylim = c(3, 4.1)) +
  theme_nature()

ggsave(file.path(out_dir, "full_income_quintile_pooled.png"), width = 10, height = 6, dpi = 300)

# --- Separate models plot (uses lightweight coefs — no model objects reloaded) ---
plot_coefs_separate <- readRDS(file.path(out_dir, "plot_coefs_separate.rds"))

predict_separate <- function(row, hi_seq) {
  coef_p2 <- if (is.na(row$coef_precip2)) 0 else row$coef_precip2
  tibble(
    HI_F_new        = hi_seq,
    income_quintile = row$quintile,
    HI_below        = pmin(hi_seq, row$breakpoint),
    HI_above        = pmax(0, hi_seq - row$breakpoint),
    breakpoint      = row$breakpoint
  ) %>%
    mutate(
      fit = row$slope_below * HI_below +
        row$slope_above * HI_above +
        row$coef_precip  * mean(df$Precipitation,   na.rm = TRUE) +
        coef_p2          * mean(df$Precipitation^2, na.rm = TRUE) +
        row$coef_wind    * mean(df$Wind_Speed,      na.rm = TRUE)
    )
}

# FIX: compute actual means as a clean lookup table to avoid fragile
# factor comparison inside group_by mutate, which can silently return NAs
# Named vector lookup — avoids any factor/character join mismatch
actual_means_vec <- df %>%
  group_by(income_quintile) %>%
  summarise(m = mean(log_consumption, na.rm = TRUE), .groups = "drop") %>%
  mutate(income_quintile = as.character(income_quintile)) %>%
  { setNames(.$m, .$income_quintile) }

pred_separate <- map_df(
  split(plot_coefs_separate, plot_coefs_separate$quintile),
  ~predict_separate(.x, seq(90, 115, by = 0.5))
) %>%
  mutate(
    actual_mean     = actual_means_vec[income_quintile],
    income_quintile = factor(income_quintile, levels = levels(df$income_quintile))
  ) %>%
  group_by(income_quintile) %>%
  mutate(fit_aligned = fit + (actual_mean - mean(fit, na.rm = TRUE))) %>%
  ungroup()

bp_points_separate <- pred_separate %>%
  group_by(income_quintile) %>%
  slice(which.min(abs(HI_F_new - breakpoint))) %>% ungroup()

ggplot(pred_separate, aes(x = HI_F_new, y = fit_aligned, group = income_quintile)) +
  geom_line(aes(linetype = income_quintile), color = "black", linewidth = 1) +
  geom_point(data = bp_points_separate, aes(fill = income_quintile),
             shape = 21, size = 6, stroke = 0.5, alpha = 0.7, color = "black") +
  scale_linetype_manual(values = line_types, name = "Income Quintile") +
  scale_fill_manual(values = nature_palette, name = "Income Quintile") +
  scale_x_continuous(breaks = seq(90, 115, by = 5)) +
  scale_y_continuous(breaks = seq(3, 4.1, by = 0.2)) +
  guides(linetype = guide_legend(override.aes = list(linewidth = 1), keywidth = unit(1.5, "cm")),
         fill     = guide_legend(override.aes = list(size = 4))) +
  labs(x = "Heat Index (°F)", y = "Log of Daily Electricity Consumption") +
  coord_cartesian(xlim = c(90, 115), ylim = c(3, 4.1)) +
  theme_nature()

ggsave(file.path(out_dir, "full_income_quintile_separate.png"), width = 10, height = 6, dpi = 300)
### ============================================================
###   SUMMARY TABLES + FINAL RECOMMENDATION
### ============================================================

separate_summary <- read_csv(file.path(out_dir, "separate_summary.csv"))

pooled_coef_summary <- tibble(
  income_quintile = c("Q1: 0-20%","Q2: 20-40%","Q3: 40-60%","Q4: 60-80%","Q5: 80-100%"),
  slope_below = round(coef(fe_pooled)[coefs_below], 4),
  se_below    = round(sqrt(diag(vcov(fe_pooled)))[coefs_below], 4),
  slope_above = round(coef(fe_pooled)[coefs_above], 4),
  se_above    = round(sqrt(diag(vcov(fe_pooled)))[coefs_above], 4)
)

cat("\n=== POOLED MODEL COEFFICIENTS ===\n"); print(pooled_coef_summary)
cat("\nSeparate breakpoints:", paste(round(separate_summary$breakpoint, 5), collapse = ", "), "°F\n")
cat("Pooled breakpoint:", round(bp_pooled, 5), "°F\n")

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
  cat("\n✓ USE: POOLED MODEL — breakpoint =", round(bp_pooled, 4), "°F\n")
  print(pooled_coef_summary)
  cat("\nWald test results: see output above\n")
}

print(plot_coefs_separate)

test_row <- split(plot_coefs_separate, plot_coefs_separate$quintile)[[1]]
test_pred <- predict_separate(test_row, seq(90, 115, by = 0.5))
print(summary(test_pred$fit))
print(summary(pred_separate$actual_mean))

### Average median hh income 
income_summary <- df %>%
  distinct(PREM_ID, income_quintile, median_hh_income) %>%
  group_by(income_quintile) %>%
  summarise(
    avg_median_income = mean(median_hh_income, na.rm = TRUE),
    median_income     = median(median_hh_income, na.rm = TRUE),
    min_income        = min(median_hh_income, na.rm = TRUE),
    max_income        = max(median_hh_income, na.rm = TRUE),
    n_households      = n()
  )

print(income_summary)


####

### ============================================================
###   TABLE F3: Pooled Model with Income Quintile Interactions
### ============================================================
library(flextable)
library(officer)

b     <- coef(fe_pooled)
se    <- sqrt(diag(vcov(fe_pooled)))
pvals <- summary(fe_pooled)$coeftable[, "Pr(>|t|)"]
fs    <- fitstat(fe_pooled, ~ n + ar2 + wr2 + rmse)

stars <- function(p) ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
### ============================================================
###   TABLE F3: Pooled Model with Income Quintile Interactions
### ============================================================
library(flextable)
library(officer)

b     <- coef(fe_pooled)
se    <- sqrt(diag(vcov(fe_pooled)))
pvals <- summary(fe_pooled)$coeftable[, "Pr(>|t|)"]
fs    <- fitstat(fe_pooled, ~ n + ar2 + wr2 + rmse)

stars <- function(p) ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
fmt   <- function(x, d = 5) sprintf(paste0("%.", d, "f"), x)

quints <- c("Q1: 0-20%","Q2: 20-40%","Q3: 40-60%","Q4: 60-80%","Q5: 80-100%")
labels <- c("Q1: Lowest 20%","Q2: 20-40%","Q3: 40-60%","Q4: 60-80%","Q5: Highest 20%")

below_names <- paste0("HI_below:income_quintile", quints)
above_names <- paste0("income_quintile", quints, ":HI_above")

# --- Build quintile rows (coef row + SE row, interleaved) ---
quint_rows <- map_df(seq_along(quints), function(i) {
  bn <- below_names[i]; an <- above_names[i]
  tibble(
    Row = c(labels[i], ""),
    `HI Below` = c(paste0(fmt(b[bn]), stars(pvals[bn])), paste0("(", fmt(se[bn]), ")")),
    `HI Above` = c(paste0(fmt(b[an]), stars(pvals[an])), paste0("(", fmt(se[an]), ")"))
  )
})

# --- Controls rows ---
precip_name  <- "Precipitation"
precip2_name <- grep("Precipitation\\^2", names(b), value = TRUE)
wind_name    <- "Wind_Speed"

controls_rows <- tibble(
  Row = c("Precipitation", "", "Precipitation²", "", "Wind Speed", ""),
  `HI Below` = c(
    paste0(fmt(b[precip_name]), stars(pvals[precip_name])), paste0("(", fmt(se[precip_name]), ")"),
    paste0(fmt(b[precip2_name]), stars(pvals[precip2_name])), paste0("(", fmt(se[precip2_name]), ")"),
    paste0(fmt(b[wind_name]), stars(pvals[wind_name])), paste0("(", fmt(se[wind_name]), ")")
  ),
  `HI Above` = ""
)

# --- Fit stats rows ---
fit_rows <- tibble(
  Row = c("Num. obs.", "Num. households", "Adj. R2", "Within R2", "RMSE",
          "Household FE", "Month FE"),
  `HI Below` = c(
    scales::comma(fs$n), scales::comma(fe_pooled$fixef_sizes["PREM_ID"]),
    fmt(fs$ar2), fmt(fs$wr2), fmt(fs$rmse), "Yes", "Yes"
  ),
  `HI Above` = ""
)

table_f3 <- bind_rows(
  tibble(Row = "", `HI Below` = "HI Below", `HI Above` = "HI Above"),
  quint_rows,
  tibble(Row = "CONTROLS", `HI Below` = "", `HI Above` = ""),
  controls_rows,
  tibble(Row = "", `HI Below` = "", `HI Above` = ""),
  fit_rows
)

# --- Render as flextable ---
ft <- flextable(table_f3) %>%
  delete_part(part = "header") %>%
  add_header_row(values = c("", "HI Below", "HI Above"), colwidths = c(1,1,1)) %>%
  bold(i = 1, part = "header") %>%
  bold(i = which(table_f3$Row %in% c(labels, "Precipitation", "Precipitation²",
                                     "Wind Speed", "CONTROLS")), j = 1) %>%
  add_footer_lines(c(
    "Standard errors clustered by household (PREM_ID) in parentheses.",
    "*** p<0.001, ** p<0.01, * p<0.05"
  )) %>%
  autofit()

print(ft, preview = "docx")
save_as_docx(ft, path = file.path(out_dir, "Table_F3_pooled_quintile.docx"))

cat("\nSaved Table F3 to:", file.path(out_dir, "Table_F3_pooled_quintile.docx"), "\n")

quints <- c("Q1: 0-20%","Q2: 20-40%","Q3: 40-60%","Q4: 60-80%","Q5: 80-100%")
labels <- c("Q1: Lowest 20%","Q2: 20-40%","Q3: 40-60%","Q4: 60-80%","Q5: Highest 20%")

below_names <- paste0("HI_below:income_quintile", quints)
above_names <- paste0("income_quintile", quints, ":HI_above")

# --- Build quintile rows (coef row + SE row, interleaved) ---
quint_rows <- map_df(seq_along(quints), function(i) {
  bn <- below_names[i]; an <- above_names[i]
  tibble(
    Row = c(labels[i], ""),
    `HI Below` = c(paste0(fmt(b[bn]), stars(pvals[bn])), paste0("(", fmt(se[bn]), ")")),
    `HI Above` = c(paste0(fmt(b[an]), stars(pvals[an])), paste0("(", fmt(se[an]), ")"))
  )
})

# --- Controls rows ---
precip_name  <- "Precipitation"
precip2_name <- grep("Precipitation\\^2", names(b), value = TRUE)
wind_name    <- "Wind_Speed"

controls_rows <- tibble(
  Row = c("Precipitation", "", "Precipitation²", "", "Wind Speed", ""),
  `HI Below` = c(
    paste0(fmt(b[precip_name]), stars(pvals[precip_name])), paste0("(", fmt(se[precip_name]), ")"),
    paste0(fmt(b[precip2_name]), stars(pvals[precip2_name])), paste0("(", fmt(se[precip2_name]), ")"),
    paste0(fmt(b[wind_name]), stars(pvals[wind_name])), paste0("(", fmt(se[wind_name]), ")")
  ),
  `HI Above` = ""
)

# --- Fit stats rows ---
fit_rows <- tibble(
  Row = c("Num. obs.", "Num. households", "Adj. R2", "Within R2", "RMSE",
          "Household FE", "Month FE"),
  `HI Below` = c(
    scales::comma(fs$n), scales::comma(fe_pooled$fixef_sizes["PREM_ID"]),
    fmt(fs$ar2), fmt(fs$wr2), fmt(fs$rmse), "Yes", "Yes"
  ),
  `HI Above` = ""
)

table_f3 <- bind_rows(
  tibble(Row = "", `HI Below` = "HI Below", `HI Above` = "HI Above"),
  quint_rows,
  tibble(Row = "CONTROLS", `HI Below` = "", `HI Above` = ""),
  controls_rows,
  tibble(Row = "", `HI Below` = "", `HI Above` = ""),
  fit_rows
)

# --- Render as flextable ---
ft <- flextable(table_f3) %>%
  delete_part(part = "header") %>%
  add_header_row(values = c("", "HI Below", "HI Above"), colwidths = c(1,1,1)) %>%
  bold(i = 1, part = "header") %>%
  bold(i = which(table_f3$Row %in% c(labels, "Precipitation", "Precipitation²",
                                     "Wind Speed", "CONTROLS")), j = 1) %>%
  add_footer_lines(c(
    "Standard errors clustered by household (PREM_ID) in parentheses.",
    "*** p<0.001, ** p<0.01, * p<0.05"
  )) %>%
  autofit()

print(ft, preview = "docx")
save_as_docx(ft, path = file.path(out_dir, "Table_F3_pooled_quintile.docx"))

cat("\nSaved Table F3 to:", file.path(out_dir, "Table_F3_pooled_quintile.docx"), "\n")
