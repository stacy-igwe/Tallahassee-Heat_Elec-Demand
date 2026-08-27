# Nature-style theme
library(showtext)
library(tidyverse)
library(ggplot2)
library(viridis)

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

slopes_df <- tribble(
  ~dataset, ~quintile, ~segment, ~slope, ~se,
  # ---- Full sample ----
  "Full", "Q1", "Below", 0.0170, 0.0001,
  "Full", "Q1", "Above", 0.0063, 0.0002,
  "Full", "Q2", "Below", 0.0187, 0.0001,
  "Full", "Q2", "Above", 0.0106, 0.0001,
  "Full", "Q3", "Below", 0.0208, 0.0001,
  "Full", "Q3", "Above", 0.0125, 0.0002,
  "Full", "Q4", "Below", 0.021, 0.0001,
  "Full", "Q4", "Above", 0.0133, 0.0002,
  "Full", "Q5", "Below", 0.0217, 0.0001,
  "Full", "Q5", "Above", 0.0151, 0.0001,
  
  # ---- Rebate sample ----
  "Rebate", "Q1", "Below", 0.0195, 0.0006,
  "Rebate", "Q1", "Above", 0.0042, 0.0018,
  "Rebate", "Q2", "Below", 0.0204, 0.0005,
  "Rebate", "Q2", "Above", 0.0124, 0.0006,
  "Rebate", "Q3", "Below", 0.0227, 0.0004,
  "Rebate", "Q3", "Above", 0.0144, 0.0004,
  "Rebate", "Q4", "Below", 0.0209, 0.0002,
  "Rebate", "Q4", "Above", 0.0141, 0.0005,
  "Rebate", "Q5", "Below", 0.0216, 0.0002,
  "Rebate", "Q5", "Above", 0.0163, 0.0003
)

set.seed(123)
plot_df <- slopes_df %>%
  group_by(dataset, quintile, segment) %>%
  do({
    tibble(
      value = rnorm(1000, mean = .$slope, sd = .$se)
    )
  }) %>%
  ungroup()

plot_df <- plot_df %>%
  mutate(
    dataset = factor(dataset, levels = c("Full", "Rebate")),
    quintile = factor(quintile, levels = paste0("Q", 1:5)),
    segment = factor(segment, levels = c("Below", "Above"))
  )

# ========================================
# FIGURE 1: Below Breakpoint Slopes
# ========================================
plot_below <- plot_df %>%
  filter(segment == "Below") %>%
  ggplot(aes(x = quintile, y = value, fill = dataset)) +
  
  
  geom_boxplot(
    width = 0.6,
    outlier.shape = NA,  # You have this
    linewidth = 0.4,
    position = position_dodge(width = 0.75)
  ) +
  
  scale_fill_viridis_d(
    option = "rocket",
    begin = 0.40,
    end   = 0.80,
    direction = 1, alpha = .7,
    labels = c("Full Sample", "Rebate Sample")
  ) +
  
  
  # scale_fill_manual(
  #   values = box_palette,
  #   labels = c("Full Sample", "Rebate Sample")
  # ) +
  
  # Add a horizontal line at y = 0 to highlight the negative Q1 Full value
  geom_hline(yintercept = 0, linetype = "dashed", 
             color = "gray40", linewidth = 0.3) +
  
  labs(
    title = "Below Breakpoint",
    x = "Income Quintile",
    y = "Slope (% change in electricity use per °F)",
    fill = "Sample"
  ) +
  
  theme_nature() +
  coord_cartesian(ylim = c(0.015, .025)) +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, size = 12),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(plot_below)

ggsave(
  filename = "figure_below_breakpoint.png",
  plot     = plot_below,
  width    = 8,
  height   = 12,
  units    = "cm",
  dpi      = 300,
  bg       = "white"
)


# ========================================
# FIGURE 2: Above Breakpoint Slopes
# ========================================
# Enhanced version with some refinements

box_palette <- c(
  "Full"   = "#9055A2",  # deep blue-gray (clean, strong anchor)
  "Rebate" = "#D37256"   # modern coral
)


plot_above <- plot_df %>%
  filter(segment == "Above") %>%
  ggplot(aes(x = quintile, y = value, fill = dataset)) +
  
  geom_boxplot(
    width = 0.6,
    outlier.shape = NA,  # You have this
    linewidth = 0.4,
    position = position_dodge(width = 0.75)
  ) +
  
  scale_fill_viridis_d(
    option = "rocket",
    begin = 0.40,
    end   = 0.80,
    direction = 1, alpha = .7,
    labels = c("Full Sample", "Rebate Sample")
  ) +
  
  
  # scale_fill_manual(
  #   values = box_palette,
  #   labels = c("Full Sample", "Rebate Sample")
  # ) +
  
  # Add a horizontal line at y = 0 to highlight the negative Q1 Full value
  geom_hline(yintercept = 0, linetype = "dashed", 
             color = "gray40", linewidth = 0.3) +
  
  labs(
    title = "Above Breakpoint",
    x = "Income Quintile",
    y = "Slope (% change in electricity use per °F)",
    fill = "Sample"
  ) +
  
  theme_nature() +
  coord_cartesian(ylim = c(-.003,.017)) +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, size = 12),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )



print(plot_above)

ggsave(
  filename = "figure_above_breakpoint-7.14.png",
  plot     = plot_above,
  width    = 7,
  height   = 5,
  units    = "in",
  dpi      = 300,
  bg       = "white"
)

# Nature-style theme
library(showtext)
library(tidyverse)
library(ggplot2)
library(viridis)

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

slopes_df <- tribble(
  ~dataset, ~quintile, ~segment, ~slope, ~se,
  # ---- Full sample ----
  "Full", "Q1", "Below", 0.0170, 0.0001,
  "Full", "Q1", "Above", 0.0063, 0.0002,
  "Full", "Q2", "Below", 0.0187, 0.0001,
  "Full", "Q2", "Above", 0.0106, 0.0001,
  "Full", "Q3", "Below", 0.0208, 0.0001,
  "Full", "Q3", "Above", 0.0125, 0.0002,
  "Full", "Q4", "Below", 0.021, 0.0001,
  "Full", "Q4", "Above", 0.0133, 0.0002,
  "Full", "Q5", "Below", 0.0217, 0.0001,
  "Full", "Q5", "Above", 0.0151, 0.0001,
  
  # ---- Rebate sample ----
  "Rebate", "Q1", "Below", 0.0195, 0.0006,
  "Rebate", "Q1", "Above", 0.0042, 0.0018,
  "Rebate", "Q2", "Below", 0.0204, 0.0005,
  "Rebate", "Q2", "Above", 0.0124, 0.0006,
  "Rebate", "Q3", "Below", 0.0227, 0.0004,
  "Rebate", "Q3", "Above", 0.0144, 0.0004,
  "Rebate", "Q4", "Below", 0.0209, 0.0002,
  "Rebate", "Q4", "Above", 0.0141, 0.0005,
  "Rebate", "Q5", "Below", 0.0216, 0.0002,
  "Rebate", "Q5", "Above", 0.0163, 0.0003
)

set.seed(123)
plot_df <- slopes_df %>%
  group_by(dataset, quintile, segment) %>%
  do({
    tibble(
      value = rnorm(1000, mean = .$slope, sd = .$se)
    )
  }) %>%
  ungroup()

plot_df <- plot_df %>%
  mutate(
    dataset = factor(dataset, levels = c("Full", "Rebate")),
    quintile = factor(quintile, levels = paste0("Q", 1:5)),
    segment = factor(segment, levels = c("Below", "Above"))
  )

# ========================================
# FIGURE 1: Below Breakpoint Slopes
# ========================================
plot_below <- plot_df %>%
  filter(segment == "Below") %>%
  ggplot(aes(x = quintile, y = value, fill = dataset)) +
  
  
  geom_boxplot(
    width = 0.6,
    outlier.shape = NA,  # You have this
    linewidth = 0.4,
    position = position_dodge(width = 0.75)
  ) +
  
  scale_fill_viridis_d(
    option = "rocket",
    begin = 0.40,
    end   = 0.80,
    direction = 1, alpha = .7,
    labels = c("Full Sample", "Rebate Sample")
  ) +
  
  
  # scale_fill_manual(
  #   values = box_palette,
  #   labels = c("Full Sample", "Rebate Sample")
  # ) +
  
  # Add a horizontal line at y = 0 to highlight the negative Q1 Full value
  geom_hline(yintercept = 0, linetype = "dashed", 
             color = "gray40", linewidth = 0.3) +
  
  labs(
    title = "Below Breakpoint",
    x = "Income Quintile",
    y = "Slope (% change in electricity use per °F)",
    fill = "Sample"
  )  +
  
  theme_nature() +
  coord_cartesian(ylim = c(0.015, .025)) +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, size = 12),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(plot_below)

ggsave(
  filename = "figure_below_breakpoint.png",
  plot     = plot_below,
  width    = 8,
  height   = 12,
  units    = "cm",
  dpi      = 300,
  bg       = "white"
)


# ========================================
# FIGURE 2: Above Breakpoint Slopes
# ========================================
# Enhanced version with some refinements

box_palette <- c(
  "Full"   = "#9055A2",  # deep blue-gray (clean, strong anchor)
  "Rebate" = "#D37256"   # modern coral
)


plot_above <- plot_df %>%
  filter(segment == "Above") %>%
  ggplot(aes(x = quintile, y = value, fill = dataset)) +
  
  geom_boxplot(
    width = 0.6,
    outlier.shape = NA,  # You have this
    linewidth = 0.4,
    position = position_dodge(width = 0.75)
  ) +
  
  scale_fill_viridis_d(
    option = "rocket",
    begin = 0.40,
    end   = 0.80,
    direction = 1, alpha = .7,
    labels = c("Full Sample", "Rebate Sample")
  ) +
  
  
  # scale_fill_manual(
  #   values = box_palette,
  #   labels = c("Full Sample", "Rebate Sample")
  # ) +
  
  # Add a horizontal line at y = 0 to highlight the negative Q1 Full value
  geom_hline(yintercept = 0, linetype = "dashed", 
             color = "gray40", linewidth = 0.3) +
  
  labs(
    title = "Above Breakpoint",
    x = "Income Quintile",
    y = "Slope (% change in electricity use per °F)",
    fill = "Sample"
  )  +
  
  theme_nature() +
  coord_cartesian(ylim = c(-.003,.017)) +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, size = 12),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )



print(plot_above)

ggsave(
  filename = "figure_above_breakpoint-7.14.png",
  plot     = plot_above,
  width    = 7,
  height   = 5,
  units    = "in",
  dpi      = 300,
  bg       = "white"
)
library(patchwork)

final_plot <- plot_below + plot_above +
  plot_layout(
    ncol = 2,
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom"
  )

print(final_plot)

ggsave(
  filename = "figure_breakpoint_slopes_side_by_side.png",
  plot = final_plot,
  width = 12,
  height = 5,
  units = "in",
  dpi = 300,
  bg = "white"
)



