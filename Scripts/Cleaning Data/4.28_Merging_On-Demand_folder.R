### This file takes the original data (split by months) and combines the folder of .csv files and cuts to just the summer monts
## Note I only have summer data in the folder 

library(tidyverse)
library(readr)


# Folder containing raw 2019 CSV files
folder_path <- "~/Library/Mobile Documents/com~apple~CloudDocs/April Complete Reset/Data/Original Files/OnDemand_Clean-Data"

# Output file
output_path <- "~/Library/Mobile Documents/com~apple~CloudDocs/April Complete Reset/Data/Data-Output/final_merged_data_summer-2019.csv"

# Get file list
files_2019 <- list.files(
  path = folder_path,
  pattern = "\\.csv$",
  full.names = TRUE
) %>% sort()

# Merge
merged_data_2019 <- map_dfr(files_2019, ~ read_csv(.x, show_col_types = FALSE))

# Create HOUSE_ID + filter dates
merged_data_2019_summer <- merged_data_2019 %>%
  mutate(
    DATE = as.Date(DATE),
    HOUSE_ID = paste(PREM_ID, ACCT_ID, sep = "_")
  ) %>%
  filter(
    DATE >= as.Date("2019-05-01"),
    DATE <= as.Date("2019-09-30")
  )

# Checks
merged_data_2019_summer %>%
  summarise(
    rows = n(),
    unique_households = n_distinct(HOUSE_ID),
    unique_prem = n_distinct(PREM_ID),
    unique_acct = n_distinct(ACCT_ID),
    min_date = min(DATE),
    max_date = max(DATE)
  )

# Save
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
write_csv(merged_data_2019_summer, output_path)

head(merged_data_2019_summer)
