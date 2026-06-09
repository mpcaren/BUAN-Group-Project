project_colors <- list(
  ink = "#2F3437",
  muted = "#697077",
  caption = "#8A8F98",
  grid = "#D9DEE5",
  math = "#3B82A0",
  ela = "#2A9D8F",
  teal = "#2A9D8F",
  green = "#79A857",
  amber = "#E9A93A",
  coral = "#D65F5F",
  navy = "#243B53",
  plum = "#7C5A8A"
)

subject_colors <- c(
  "Math" = project_colors$math,
  "ELA" = project_colors$ela
)

wealth_poverty_colors <- c(
  "High-wealth (low poverty)" = project_colors$teal,
  "High-wealth\n(low poverty)" = project_colors$teal,
  "High-poverty" = project_colors$coral
)

poverty_tier_colors <- c(
  "Low poverty\n(wealthy)" = project_colors$teal,
  "Low-mid\npoverty" = project_colors$green,
  "High-mid\npoverty" = project_colors$amber,
  "High\npoverty" = project_colors$coral
)

salary_quartile_colors <- c(
  "Q1 (Lowest pay)" = "#A8DADC",
  "Q2" = project_colors$math,
  "Q3" = project_colors$navy,
  "Q4 (Highest pay)" = project_colors$plum
)

free_lunch_quartile_colors <- c(
  "Q1: lowest free lunch" = project_colors$teal,
  "Q2" = project_colors$green,
  "Q3" = project_colors$amber,
  "Q4: highest free lunch" = project_colors$coral
)

period_colors <- c(
  "Pre-Covid (2015-2019)" = project_colors$teal,
  "Post-Covid (2022-2025)" = project_colors$math
)

theme_project <- function(base_size = 13, legend_position = "top") {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = base_size + 2,
        color = project_colors$ink,
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        color = project_colors$muted,
        size = base_size - 3,
        margin = margin(b = 14)
      ),
      plot.caption = element_text(
        color = project_colors$caption,
        size = base_size - 4,
        margin = margin(t = 10)
      ),
      plot.margin = margin(16, 20, 12, 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = project_colors$grid, linewidth = 0.5),
      axis.ticks = element_line(color = project_colors$grid, linewidth = 0.4),
      axis.ticks.length = unit(0.2, "cm"),
      axis.text = element_text(color = project_colors$muted, size = base_size - 3),
      axis.title = element_text(color = project_colors$muted, size = base_size - 3),
      strip.text = element_text(face = "bold", color = project_colors$ink, size = base_size - 1),
      legend.position = legend_position,
      legend.title = element_text(face = "bold", color = project_colors$ink, size = base_size - 3),
      legend.text = element_text(color = project_colors$ink, size = base_size - 3),
      legend.key.size = unit(0.8, "cm")
    )
}
