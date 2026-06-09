library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
source(file.path("Scripts", "visualizations", "plot_style.R"))

data_path <- file.path(
  "Data",
  "converted",
  "1. Data for Student Performance Paper 2015 2025.csv"
)
output_dir <- file.path("Outputs", "figures")
output_path <- file.path(output_dir, "viz4_wealth_poverty_profiles.png")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

df <- read.csv(data_path, stringsAsFactors = FALSE)

df_clean <- df %>%
  filter(!is.na(pctfrpl), pctfrpl >= 0, pctfrpl <= 1) %>%
  filter(!is.na(meantotalsalary_cpi_adjusted), meantotalsalary_cpi_adjusted > 0) %>%
  filter(!is.na(metMATH_pct), !is.na(metELA_pct))

poverty_cuts <- quantile(df_clean$pctfrpl, c(0.25, 0.75), na.rm = TRUE)

df_typed <- df_clean %>%
  mutate(district_type = case_when(
    pctfrpl <= poverty_cuts[1] ~ "High-wealth districts",
    pctfrpl >= poverty_cuts[2] ~ "High-poverty districts",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(district_type))

summary_df <- df_typed %>%
  group_by(district_type) %>%
  summarise(
    `Math scores` = mean(metMATH_pct, na.rm = TRUE) * 100,
    `ELA scores` = mean(metELA_pct, na.rm = TRUE) * 100,
    `Mean teacher salary` = mean(meantotalsalary_cpi_adjusted, na.rm = TRUE),
    .groups = "drop"
  )

scores_df <- summary_df %>%
  select(district_type, `Math scores`, `ELA scores`) %>%
  pivot_longer(-district_type, names_to = "Metric", values_to = "value") %>%
  mutate(
    panel = "Test scores (% meeting standard)",
    display = paste0(round(value, 1), "%")
  )

salary_df <- summary_df %>%
  select(district_type, `Mean teacher salary`) %>%
  pivot_longer(-district_type, names_to = "Metric", values_to = "value") %>%
  mutate(
    panel = "Mean teacher salary (2015 dollars)",
    display = paste0("$", round(value / 1000, 1), "K")
  )

combined <- bind_rows(scores_df, salary_df) %>%
  mutate(
    panel = factor(
      panel,
      levels = c(
        "Test scores (% meeting standard)",
        "Mean teacher salary (2015 dollars)"
      )
    ),
    district_type = factor(
      district_type,
      levels = c("High-wealth districts", "High-poverty districts")
    )
  )

district_colors <- c(
  "High-wealth districts" = project_colors$teal,
  "High-poverty districts" = project_colors$coral
)

p <- ggplot(combined, aes(x = Metric, y = value, fill = district_type)) +
  geom_col(position = position_dodge(width = 0.65), width = 0.55) +
  geom_text(
    aes(label = display),
    position = position_dodge(width = 0.65),
    vjust = -0.5,
    size = 3.5,
    fontface = "bold"
  ) +
  facet_wrap(~panel, scales = "free", ncol = 2) +
  scale_fill_manual(values = district_colors, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "The Pay Gap is Small. The Learning Gap is Not.",
    subtitle = "Averages across 2015-2025  |  High-poverty = top poverty quartile  |  High-wealth = bottom poverty quartile  |  Dollars are CPI-adjusted to 2015",
    x = NULL,
    y = NULL,
    caption = NULL
  ) +
  theme_project(base_size = 12, legend_position = "bottom") +
  theme(
    axis.text.x = element_text(size = 11, angle = 0, hjust = 0.5),
    axis.text.y = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  )

ggsave(
  output_path,
  plot = p,
  width = 10,
  height = 6,
  dpi = 180,
  bg = "white"
)
cat("Saved.\n")
