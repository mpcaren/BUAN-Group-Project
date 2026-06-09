required_packages <- c(
  "dplyr",
  "ggplot2",
  "readr",
  "scales",
  "stringr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(stringr)
source(file.path("Scripts", "visualizations", "plot_style.R"))

report_card_path <- file.path(
  "Data",
  "converted",
  "ospi_report_card_teacher_experience_distribution_lea.csv"
)

output_dir <- file.path("Outputs", "figures")
output_path <- file.path(
  output_dir,
  "mean_teacher_experience_timeseries_report_card.png"
)

if (!file.exists(report_card_path)) {
  stop("Could not find OSPI Report Card teacher experience data: ", report_card_path)
}

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

experience_midpoints <- c(
  "0.0 - 4.9" = 2.45,
  "5.0 - 9.9" = 7.45,
  "10.0 - 14.9" = 12.45,
  "15.0 - 19.9" = 17.45,
  "20.0 - 24.9" = 22.45,
  "25.0 - 29.9" = 27.45,
  "30.0 - 34.9" = 32.45,
  "35.0 - 39.9" = 37.45,
  "40.0 - 44.9" = 42.45,
  "45.0 - 49.9" = 47.45,
  "50.0 - 54.9" = 52.45,
  "55.0 - 59.9" = 57.45
)

plot_data <- read_csv(report_card_path, show_col_types = FALSE) |>
  mutate(
    year = as.integer(str_sub(schoolyear, -2, -1)) + 2000L,
    teacher_count = as.numeric(teachercount),
    bin_midpoint = unname(experience_midpoints[experiencebin])
  ) |>
  filter(!is.na(bin_midpoint), !is.na(teacher_count)) |>
  group_by(year) |>
  summarize(
    mean_teacher_experience = sum(bin_midpoint * teacher_count, na.rm = TRUE) /
      sum(teacher_count, na.rm = TRUE),
    reported_teachers = sum(teacher_count, na.rm = TRUE),
    .groups = "drop"
  )

teacher_experience_plot <- ggplot(
  plot_data,
  aes(x = year, y = mean_teacher_experience)
) +
  geom_line(color = project_colors$teal, linewidth = 1.2) +
  geom_point(color = project_colors$teal, size = 2.7) +
  scale_x_continuous(breaks = sort(unique(plot_data$year))) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.04, 0.08))
  ) +
  labs(
    title = "Mean Teacher Experience Over Time",
    subtitle = "Estimated from OSPI Report Card teacher experience bins",
    x = "Year",
    y = "Estimated mean teacher experience",
    caption = NULL
  ) +
  theme_project(base_size = 13, legend_position = "none")

ggsave(
  filename = output_path,
  plot = teacher_experience_plot,
  width = 9,
  height = 5.5,
  dpi = 300
)

if (interactive()) {
  print(teacher_experience_plot)
}

print(plot_data)
message("Saved graph to: ", normalizePath(output_path, mustWork = FALSE))
