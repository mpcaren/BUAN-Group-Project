library(ggplot2)
library(dplyr)
library(scales)
source(file.path("Scripts", "visualizations", "plot_style.R"))

data_path <- file.path(
  "Data",
  "converted",
  "1. Data for Student Performance Paper 2015 2025.csv"
)
output_dir <- file.path("Outputs", "figures")
output_path <- file.path(output_dir, "wa_salary_vs_scores_scatter.png")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

df <- read.csv(data_path, stringsAsFactors = FALSE)

df_clean <- df %>%
  filter(!is.na(meantotalsalary_cpi_adjusted), meantotalsalary_cpi_adjusted > 0) %>%
  filter(!is.na(metMATH_pct), !is.na(metELA_pct)) %>%
  mutate(composite = ((metMATH_pct + metELA_pct) / 2) * 100)

r_val <- round(cor(
  df_clean$meantotalsalary_cpi_adjusted,
  df_clean$composite,
  use = "complete.obs"
), 2)

p <- ggplot(df_clean, aes(x = meantotalsalary_cpi_adjusted, y = composite)) +
  geom_point(size = 1.8, shape = 16, color = project_colors$math, alpha = 0.25) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 1.1,
    color = project_colors$ink,
    fill = project_colors$grid,
    alpha = 0.15
  ) +
  annotate(
    "text",
    x = quantile(df_clean$meantotalsalary_cpi_adjusted, 0.97, na.rm = TRUE),
    y = 8,
    label = paste0("r = ", r_val),
    hjust = 1,
    size = 3.8,
    fontface = "bold",
    color = project_colors$ink
  ) +
  scale_x_continuous(
    labels = dollar_format(scale = 1e-3, suffix = "K"),
    breaks = seq(20000, 160000, by = 20000)
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)
  ) +
  labs(
    title = "Higher teacher salaries do not increase student outcomes in Washington State, 2015-2025",
    subtitle = "Each point = one district in one year  |  Dollars are CPI-adjusted to 2015",
    x = "Mean total teacher salary",
    y = "Students meeting standard (%)",
    caption = NULL
  ) +
  theme_project(base_size = 13, legend_position = "none")

ggsave(
  output_path,
  plot = p,
  width = 10,
  height = 7,
  dpi = 180,
  bg = "white"
)
cat("Saved.\n")
