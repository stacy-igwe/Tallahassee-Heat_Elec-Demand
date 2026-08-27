### --- Load Libraries ---
library(tidyverse)
library(lubridate)
library(ggplot2)

### --- Load Full 2019 Dataset (no quintiles needed) ---
merge_ewhi_2019 <-read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/April Complete Reset/Data/Data-Output/Legit Data Output/5.12_HI_clean_merged_weather_quintiles_2019_CORRECT.csv")
colnames(merge_ewhi_2019)
### --- Filter complete summer households + positive consumption ---
avg_energy_by_hi_complete <- merge_ewhi_2019 %>%
  filter(summer_status == "complete", Daily_Consumption > 0) %>%
  group_by(HI_F_New) %>%
  summarise(
    mean_consumption = mean(Daily_Consumption, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(HI_F_New)


### --- Recreate Smoothed LOESS Plot ---
ggplot(avg_energy_by_hi_complete, aes(x = HI_F_New, y = mean_consumption)) +
  geom_smooth(
    method = "loess",
    se = TRUE,
    color = "darkred",
    fill = "pink",
    linewidth = 1.2
  ) + 
  geom_vline(xintercept = 105.74, linetype = "dashed", color = "black", linewidth = 0.9) + 
  
  annotate("text",
           x = 105.74,
           y = 50.2,      # ↖ adjust up a tiny bit
           label = "Breakpoint: 105.74°F",
           hjust = -.1, size = 4, vjust = -.75) +
  labs(
    x = "Heat Index (°F)",
    y = "Average Daily Electricity Consumption (kWh)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(), axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

### --- VERSION FOR ADVISOR ---
loess_curve <-ggplot(avg_energy_by_hi_complete, aes(x = HI_F_New, y = mean_consumption)) +
  geom_smooth(
    method = "loess",
    se = TRUE,
    color = "darkred",
    fill = "pink",
    linewidth = .7
  ) + 
  labs(
    x = "Heat Index (°F)",
    y = "Average Daily Electricity Consumption (kWh)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(), axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

loess_curve
ggsave(
  "loess_curve_red.png",
  plot = loess_curve,
  bg = "transparent",
  dpi = 600,
  width = 7,
  height = 7
)

n_distinct(merge_ewhi_2019$PREM_ID)
