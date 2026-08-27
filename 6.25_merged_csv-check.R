#code to check on the merged file??? 
library(readr)
library(dplyr)

check_csv <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/April Complete Reset/Data/Data-Output/Legit Data Output/5.12_HI_clean_merged_weather_quintiles_2019_CORRECT.csv")
head(check_csv)

n_distinct(check_csv$PREM_ID)
n_distinct(check_csv$n_summer_days)

n_distinct(df$PREM_ID)
n_distinct(df$ACCT_ID)


library(dplyr)

df %>%
  group_by(PREM_ID) %>%
  summarize(
    n_accounts = n_distinct(ACCT_ID),
    .groups = "drop"
  ) %>%
  count(n_accounts)

df %>%
  count(PREM_ID, DATE) %>%
  filter(n > 1)

setdiff(names(check_csv), names(df))

names(check_csv)
names(df)

###
hello <- df %>%
  group_by(PREM_ID, ACCT_ID) %>%
  summarize(start_date = min(DATE), end_date = max(DATE), .groups = "drop") %>%
  group_by(PREM_ID) %>%
  filter(n() > 1) %>%
  arrange(PREM_ID, start_date) %>%
  mutate(prev_end = lag(end_date), overlap = start_date <= prev_end) %>%
  filter(overlap == TRUE) %>%
  count(prev_end) %>%
  arrange(desc(n))
