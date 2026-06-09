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
output_path <- file.path(output_dir, "viz_spend_by_poverty.png")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

df <- read.csv(data_path, stringsAsFactors = FALSE)

df_clean <- df %>%
  filter(!is.na(tinstruction_cpi_adjusted), !is.na(enrollment), enrollment > 0) %>%
  filter(!is.na(pctfrpl), pctfrpl >= 0, pctfrpl <= 1) %>%
  mutate(spend_per_pupil = tinstruction_cpi_adjusted / enrollment) %>%
  filter(
    spend_per_pupil >= quantile(spend_per_pupil, 0.025),
    spend_per_pupil <= quantile(spend_per_pupil, 0.975)
  )

poverty_cuts <- quantile(df_clean$pctfrpl, c(0.25, 0.50, 0.75), na.rm = TRUE)

df_clean <- df_clean %>%
  mutate(
    poverty_tier = case_when(
      pctfrpl <= poverty_cuts[1] ~ "Low poverty\n(wealthy)",
      pctfrpl <= poverty_cuts[2] ~ "Low-mid\npoverty",
      pctfrpl <= poverty_cuts[3] ~ "High-mid\npoverty",
      TRUE ~ "High\npoverty"
    ),
    poverty_tier = factor(
      poverty_tier,
      levels = c(
        "Low poverty\n(wealthy)",
        "Low-mid\npoverty",
        "High-mid\npoverty",
        "High\npoverty"
      )
    )
  )

yearly <- df_clean %>%
  group_by(year, poverty_tier) %>%
  summarise(avg_spend = mean(spend_per_pupil, na.rm = TRUE), .groups = "drop")

p <- ggplot(yearly, aes(x = year, y = avg_spend, color = poverty_tier, group = poverty_tier)) +
  annotate(
    "rect",
    xmin = 2019.5,
    xmax = 2021.5,
    ymin = -Inf,
    ymax = Inf,
    fill = project_colors$caption,
    alpha = 0.08
  ) +
  annotate(
    "text",
    x = 2020.5,
    y = max(yearly$avg_spend) * 0.97,
    label = "Covid",
    size = 3,
    color = project_colors$muted,
    fontface = "italic"
  ) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5) +
  geom_text(
    data = yearly %>% group_by(poverty_tier) %>% slice_max(year),
    aes(label = poverty_tier),
    hjust = -0.08,
    size = 2.9,
    fontface = "bold",
    show.legend = FALSE,
    lineheight = 0.85
  ) +
  scale_color_manual(values = poverty_tier_colors, name = NULL) +
  scale_x_continuous(
    breaks = seq(min(yearly$year), max(yearly$year), by = 1),
    expand = expansion(mult = c(0.03, 0.28))
  ) +
  scale_y_continuous(
    labels = dollar_format(),
    breaks = seq(6000, 20000, by = 2000)
  ) +
  labs(
    title = "Where is the money being spent? Instructional spending by district poverty level",
    subtitle = "Average instructional spending per student by year, grouped by poverty quartile  |  Top/bottom 2.5% removed  |  Dollars are CPI-adjusted to 2015",
    x = "School year",
    y = "Avg. instructional spending per student",
    caption = NULL
  ) +
  theme_project(base_size = 13, legend_position = "none") +
  theme(
    legend.position = "none"
  )

ggsave(output_path,
  plot = p, width = 11, height = 7, dpi = 180, bg = "white"
)
cat("Saved.\n")
