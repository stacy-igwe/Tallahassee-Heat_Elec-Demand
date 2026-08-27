#### PREM_ID ONLY
#### Filters premises with consumption > 0 for all 153 summer days (May–Sept 2019)

library(data.table)
library(lubridate)

# ── 1. Load only needed columns ───────────────────────────────────────────────
cols_needed <- c("PREM_ID", "ACCT_ID", "SA_ID", "HOUSE_ID", "GEOID",
                 "DATE", "Daily_Consumption")

data_2019_summer_1 <- fread(
  "~/Library/Mobile Documents/com~apple~CloudDocs/April Complete Reset/Data/Data-Output/Legit Data Output/final_merged_data_summer-2019.csv",
  select = cols_needed
)

# Parse dates and filter in place (no copy)
data_2019_summer_1[, DATE := as.Date(DATE)]
data_2019_summer_1 <- data_2019_summer_1[DATE >= as.Date("2019-05-01") &
                                           DATE <= as.Date("2019-09-30")]
gc()

# ── 2. Quick raw check ────────────────────────────────────────────────────────
cat("Rows:", nrow(data_2019_summer_1),
    "\nPrem IDs:", uniqueN(data_2019_summer_1$PREM_ID),
    "\nUnique dates:", uniqueN(data_2019_summer_1$DATE), "\n")

# ── 3. Collapse to one row per PREM_ID-DATE ───────────────────────────────────
prem_daily_2019 <- data_2019_summer_1[
  ,
  .(
    Daily_Consumption = sum(Daily_Consumption, na.rm = TRUE),
    ACCT_ID  = ACCT_ID[1],
    SA_ID    = SA_ID[1],
    HOUSE_ID = HOUSE_ID[1],
    GEOID    = GEOID[1]
  ),
  by = .(PREM_ID, DATE)
]

# Free the raw load immediately
rm(data_2019_summer_1); gc()

# ── 4. Flag complete premises (153 days, all with consumption > 0) ─────────────
# Key fix: apply Daily_Consumption > 0 BEFORE counting days,
# otherwise "complete" premises can still have 0-consumption rows in output
prem_daily_2019 <- prem_daily_2019[Daily_Consumption > 0]

prem_summer_status <- prem_daily_2019[
  ,
  .(n_summer_days = uniqueN(DATE)),
  by = PREM_ID
][, summer_status := fifelse(n_summer_days == 153L, "complete", "incomplete")]

# Report
prem_summer_status[, .N, by = summer_status][
  , prop := N / sum(N)
] |> print()

# ── 5. Join status and filter to complete only ────────────────────────────────
prem_daily_2019 <- prem_summer_status[prem_daily_2019, on = "PREM_ID"]
rm(prem_summer_status); gc()

clean_2019_summer_1 <- prem_daily_2019[summer_status == "complete"]
rm(prem_daily_2019); gc()

# ── 6. Final checks ───────────────────────────────────────────────────────────
cat(
  "Rows:", nrow(clean_2019_summer_1),
  "\nUnique premises:", uniqueN(clean_2019_summer_1$PREM_ID),
  "\nDate range:", format(min(clean_2019_summer_1$DATE)), "to",
  format(max(clean_2019_summer_1$DATE)),
  "\nUnique dates:", uniqueN(clean_2019_summer_1$DATE),
  "\nMax consumption:", max(clean_2019_summer_1$Daily_Consumption), "\n"
)

# ── 7. Write output ───────────────────────────────────────────────────────────
out_path <- "~/Library/Mobile Documents/com~apple~CloudDocs/April Complete Reset/Data/Data-Output/5.1_clean_153_day_merged_2019.csv"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
fwrite(clean_2019_summer_1, out_path)

names(clean_2019_summer_1)
