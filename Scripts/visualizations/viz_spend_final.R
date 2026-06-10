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
output_path <- file.path(output_dir, "viz_spend_final.png")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

df <- read.csv(data_path, stringsAsFactors = FALSE)

df_clean <- df %>%
  filter(!is.na(tinstruction_cpi_adjusted), !is.na(enrollment), enrollment > 0) %>%
  filter(!is.na(metMATH_pct), !is.na(metELA_pct)) %>%
  filter(metMATH_pct > 0, metELA_pct > 0) %>%
  filter(year != 2021) %>%
  mutate(
    spend_per_pupil = tinstruction_cpi_adjusted / enrollment,
    composite = ((metMATH_pct + metELA_pct) / 2) * 100
  ) %>%
  filter(
    spend_per_pupil >= quantile(spend_per_pupil, 0.025),
    spend_per_pupil <= quantile(spend_per_pupil, 0.975)
  )

cor_val <- round(cor(
  df_clean$spend_per_pupil,
  df_clean$composite,
  use = "complete.obs"
), 2)

p <- ggplot(df_clean, aes(x = spend_per_pupil, y = composite)) +
  geom_point(size = 1.8, alpha = 0.28, shape = 16, color = project_colors$math) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 1.4,
    color = project_colors$ink,
    fill = project_colors$grid,
    alpha = 0.2,
    aes(group = 1)
  ) +
  annotate(
    "text",
    x = quantile(df_clean$spend_per_pupil, 0.975),
    y = 8,
    label = paste0("r = ", cor_val),
    size = 3.8,
    fontface = "bold",
    color = project_colors$ink,
    hjust = 1
  ) +
  scale_x_continuous(
    labels = dollar_format(),
    breaks = seq(4000, 32000, by = 4000),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    title = "Instructional spending and test scores have a slight negative association",
    subtitle = "Each point = one district in one year  |  Descriptive association, not a causal estimate  |  2021 excluded  |  Dollars are CPI-adjusted to 2015",
    x = "Instructional spending per student",
    y = "% of students meeting standard (Math + ELA avg.)",
    caption = NULL
  ) +
  theme_project(base_size = 13, legend_position = "none") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8))
  )

ggsave(output_path,
  plot = p, width = 11, height = 7, dpi = 180, bg = "white"
)
cat("Saved.\n")
