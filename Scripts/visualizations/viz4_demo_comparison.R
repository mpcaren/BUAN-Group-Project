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
output_path <- file.path(output_dir, "viz4_demo_comparison.png")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

df <- read.csv(data_path, stringsAsFactors = FALSE)

df_clean <- df %>%
  filter(!is.na(pctfrpl), pctfrpl >= 0, pctfrpl <= 1) %>%
  filter(!is.na(pcthispanic), !is.na(pctblack), !is.na(pctwhite), !is.na(pctesl))

frpl_cuts <- quantile(df_clean$pctfrpl, c(0.25, 0.75), na.rm = TRUE)

df_typed <- df_clean %>%
  mutate(district_type = case_when(
    pctfrpl <= frpl_cuts[1] ~ "High-wealth (low poverty)",
    pctfrpl >= frpl_cuts[2] ~ "High-poverty",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(district_type))

demo_long <- df_typed %>%
  group_by(district_type) %>%
  summarise(
    Hispanic     = mean(pcthispanic, na.rm = TRUE) * 100,
    White        = mean(pctwhite,    na.rm = TRUE) * 100,
    Black        = mean(pctblack,    na.rm = TRUE) * 100,
    ESL          = mean(pctesl,      na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(-district_type, names_to = "demographic", values_to = "pct") %>%
  mutate(
    demographic  = factor(demographic, levels = c("White","Hispanic","Black","ESL")),
    district_type = factor(district_type,
                           levels = c("High-wealth (low poverty)", "High-poverty"))
  )

p <- ggplot(demo_long, aes(x = demographic, y = pct, fill = district_type)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_dodge(width = 0.6),
            vjust = -0.5, size = 3.5, fontface = "bold", color = project_colors$ink) +
  scale_fill_manual(values = wealth_poverty_colors, name = NULL) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.12)),
    breaks = seq(0, 80, by = 20)
  ) +
  labs(
    title    = "Student demographics: high-poverty vs. high-wealth districts",
    subtitle = "High-poverty = top 25% free/reduced lunch  |  High-wealth = bottom 25%  |  Averaged across all years",
    x        = NULL,
    y        = "% of student population",
    caption  = NULL
  ) +
  theme_project(base_size = 13, legend_position = "top") +
  theme(
    axis.text.x = element_text(size = 12, color = project_colors$ink, face = "bold")
  )

ggsave(output_path,
       plot = p, width = 10, height = 7, dpi = 180, bg = "white")
cat("Viz 4 saved.\n")
