library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

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
  filter(!is.na(meantotalsalary), meantotalsalary > 0) %>%
  filter(!is.na(metMATH_pct), !is.na(metELA_pct))

# Define high-poverty (top quartile frpl) and high-wealth (bottom quartile frpl)
frpl_cuts <- quantile(df_clean$pctfrpl, c(0.25, 0.75), na.rm = TRUE)

df_typed <- df_clean %>%
  mutate(district_type = case_when(
    pctfrpl <= frpl_cuts[1] ~ "High-wealth\n(low poverty)",
    pctfrpl >= frpl_cuts[2] ~ "High-poverty",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(district_type))

# Summarise key metrics
summary_df <- df_typed %>%
  group_by(district_type) %>%
  summarise(
    `Math scores`         = mean(metMATH_pct, na.rm = TRUE) * 100,
    `ELA scores`          = mean(metELA_pct,  na.rm = TRUE) * 100,
    `Mean teacher salary` = mean(meantotalsalary, na.rm = TRUE),
    `% Hispanic`          = mean(pcthispanic, na.rm = TRUE) * 100,
    `% Black`             = mean(pctblack,    na.rm = TRUE) * 100,
    `% White`             = mean(pctwhite,    na.rm = TRUE) * 100,
    `% ESL`               = mean(pctesl,      na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Separate panels: test scores + salary, demographics
scores_sal <- summary_df %>%
  select(district_type, `Math scores`, `ELA scores`) %>%
  pivot_longer(-district_type, names_to = "Metric", values_to = "value") %>%
  mutate(panel = "Test scores (% meeting standard)",
         display = paste0(round(value, 1), "%"))

salary_df <- summary_df %>%
  select(district_type, `Mean teacher salary`) %>%
  pivot_longer(-district_type, names_to = "Metric", values_to = "value") %>%
  mutate(panel = "Mean teacher salary",
         display = paste0("$", round(value / 1000, 1), "K"))

demo_df <- summary_df %>%
  select(district_type, `% Hispanic`, `% Black`, `% White`, `% ESL`) %>%
  pivot_longer(-district_type, names_to = "Metric", values_to = "value") %>%
  mutate(panel = "Student demographics",
         display = paste0(round(value, 1), "%"))

combined <- bind_rows(scores_sal, salary_df, demo_df) %>%
  mutate(
    panel = factor(panel, levels = c("Test scores (% meeting standard)",
                                     "Mean teacher salary",
                                     "Student demographics")),
    district_type = factor(district_type, levels = c("High-wealth\n(low poverty)", "High-poverty"))
  )

district_colors <- c("High-wealth\n(low poverty)" = "#1D9E75",
                     "High-poverty"               = "#E24B4A")

p <- ggplot(combined, aes(x = Metric, y = value, fill = district_type)) +
  geom_col(position = position_dodge(width = 0.65), width = 0.55) +
  geom_text(aes(label = display),
            position = position_dodge(width = 0.65),
            vjust = -0.5, size = 2.9, fontface = "bold") +
  facet_wrap(~ panel, scales = "free", ncol = 3) +
  scale_fill_manual(values = district_colors, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "High-wealth vs. high-poverty district profiles in Washington State",
    subtitle = "Averages across all years  |  High-poverty = top quartile free/reduced lunch  |  High-wealth = bottom quartile",
    x        = NULL,
    y        = NULL,
    caption  = "Source: Washington State district-level data"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, margin = margin(b = 4)),
    plot.subtitle    = element_text(color = "#5F5E5A", size = 10, margin = margin(b = 14)),
    plot.caption     = element_text(color = "#888780", size = 9, margin = margin(t = 10)),
    plot.margin      = margin(16, 20, 12, 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(color = "#5F5E5A", size = 9, angle = 15, hjust = 1),
    axis.text.y      = element_blank(),
    strip.text       = element_text(face = "bold", size = 11),
    legend.position  = "top",
    legend.text      = element_text(size = 11),
    legend.key.size  = unit(0.8, "cm")
  )

ggsave(output_path,
       plot = p, width = 13, height = 6, dpi = 180, bg = "white")
cat("Viz 4 saved.\n")
