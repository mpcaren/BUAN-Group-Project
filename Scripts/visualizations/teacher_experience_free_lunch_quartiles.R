required_packages <- c(
  "dplyr",
  "ggplot2",
  "readr",
  "scales"
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

data_rds_path <- file.path(
  "Data",
  "converted",
  "1. Data for Student Performance Paper 2015 2025.rds"
)

data_csv_path <- file.path(
  "Data",
  "converted",
  "1. Data for Student Performance Paper 2015 2025.csv"
)

output_dir <- file.path("Outputs", "figures")
output_path <- file.path(output_dir, "teacher_experience_by_free_lunch_quartile.png")

if (file.exists(data_rds_path)) {
  district_data <- readRDS(data_rds_path)
} else if (file.exists(data_csv_path)) {
  district_data <- read_csv(data_csv_path, show_col_types = FALSE)
} else {
  stop("Could not find the merged dataset in Data/converted.")
}

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

plot_data <- district_data |>
  filter(
    !is.na(year),
    !is.na(meanexperience),
    !is.na(pctfrpl)
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
      meanexperience,
      w = if_else(is.na(enrollment) | enrollment <= 0, 1, enrollment),
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
  scale_color_brewer(palette = "Dark2", name = "Free lunch quartile") +
  labs(
    title = "Teacher Experience Over Time by Free Lunch Quartile",
    subtitle = "Quartiles are based on district-year percent free/reduced-price lunch in the merged dataset",
    x = "Year",
    y = "Average teacher experience",
    caption = "Teacher experience is enrollment-weighted within each quartile-year."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

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
