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

merged_data_path <- file.path(
  "Data",
  "converted",
  "1. Data for Student Performance Paper 2015 2025.rds"
)

report_card_path <- file.path(
  "Data",
  "converted",
  "ospi_report_card_teacher_experience_distribution_lea.csv"
)

output_dir <- file.path("Outputs", "figures")
output_path <- file.path(
  output_dir,
  "teacher_experience_by_free_lunch_quartile_report_card.png"
)

if (!file.exists(merged_data_path)) {
  stop("Could not find merged project dataset: ", merged_data_path)
}

if (!file.exists(report_card_path)) {
  stop("Could not find OSPI Report Card teacher experience data: ", report_card_path)
}

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

merged_data <- readRDS(merged_data_path) |>
  mutate(
    year = as.integer(year),
    districtid = str_pad(as.character(districtid), width = 5, pad = "0")
  ) |>
  select(year, districtid, name, pctfrpl, enrollment) |>
  filter(!is.na(year), !is.na(districtid), !is.na(pctfrpl))

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

report_card_experience <- read_csv(report_card_path, show_col_types = FALSE) |>
  mutate(
    year = as.integer(str_sub(schoolyear, -2, -1)) + 2000L,
    districtid = str_pad(as.character(leacode), width = 5, pad = "0"),
    teacher_count = as.numeric(teachercount),
    bin_midpoint = unname(experience_midpoints[experiencebin])
  ) |>
  filter(!is.na(bin_midpoint), !is.na(teacher_count)) |>
  group_by(year, districtid, leaname) |>
  summarize(
    report_card_meanexperience = sum(bin_midpoint * teacher_count, na.rm = TRUE) /
      sum(teacher_count, na.rm = TRUE),
    reported_teachers = sum(teacher_count, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(reported_teachers > 0)

plot_data <- merged_data |>
  inner_join(
    report_card_experience,
    by = c("year", "districtid")
  ) |>
  mutate(
    free_lunch_quartile = ntile(pctfrpl, 4),
    free_lunch_quartile = factor(
      free_lunch_quartile,
      levels = 1:4,
      labels = c(
        "Q1: lowest free lunch",
        "Q2",
        "Q3",
        "Q4: highest free lunch"
      )
    )
  ) |>
  group_by(year, free_lunch_quartile) |>
  summarize(
    teacher_experience = weighted.mean(
      report_card_meanexperience,
      w = reported_teachers,
      na.rm = TRUE
    ),
    districts = n(),
    avg_free_lunch = mean(pctfrpl, na.rm = TRUE),
    .groups = "drop"
  )

teacher_experience_plot <- ggplot(
  plot_data,
  aes(
    x = year,
    y = teacher_experience,
    color = free_lunch_quartile,
    group = free_lunch_quartile
  )
) +
  geom_line(linewidth = 1.15) +
  geom_point(size = 2.4) +
  scale_x_continuous(breaks = sort(unique(plot_data$year))) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.04, 0.08))
  ) +
  scale_color_manual(values = free_lunch_quartile_colors, name = "Free lunch quartile") +
  labs(
    title = "Teacher Experience Over Time by Free Lunch Quartile",
    subtitle = "Teacher experience estimated from OSPI Report Card experience bins",
    x = "Year",
    y = "Estimated average teacher experience",
    caption = NULL
  ) +
  theme_project(base_size = 13, legend_position = "bottom")

ggsave(
  filename = output_path,
  plot = teacher_experience_plot,
  width = 10,
  height = 6,
  dpi = 300
)

if (interactive()) {
  print(teacher_experience_plot)
}

message("Saved graph to: ", normalizePath(output_path, mustWork = FALSE))
