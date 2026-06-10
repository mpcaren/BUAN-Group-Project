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
output_path <- file.path(output_dir, "viz3_poverty_vs_scores.png")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

df <- read.csv(data_path, stringsAsFactors = FALSE)

df_clean <- df %>%
  filter(!is.na(pctfrpl), !is.na(meantotalsalary_cpi_adjusted), meantotalsalary_cpi_adjusted > 0) %>%
  filter(!is.na(metMATH_pct) | !is.na(metELA_pct)) %>%
  filter(pctfrpl >= 0, pctfrpl <= 1)

# Salary quartile labels based on full dataset
df_clean <- df_clean %>%
  mutate(salary_quartile = ntile(meantotalsalary_cpi_adjusted, 4),
         salary_label = case_when(
           salary_quartile == 1 ~ "Q1 (Lowest pay)",
           salary_quartile == 2 ~ "Q2",
           salary_quartile == 3 ~ "Q3",
           salary_quartile == 4 ~ "Q4 (Highest pay)"
         ),
         salary_label = factor(salary_label, levels = c("Q1 (Lowest pay)","Q2","Q3","Q4 (Highest pay)")))

df_long <- df_clean %>%
  select(year, name, pctfrpl, salary_label, metMATH_pct, metELA_pct) %>%
  pivot_longer(cols = c(metMATH_pct, metELA_pct),
               names_to = "Subject", values_to = "pct_met") %>%
  filter(!is.na(pct_met), pct_met > 0) %>%
  mutate(Subject = recode(Subject, metMATH_pct = "Math", metELA_pct = "ELA"),
         pct_met = pct_met * 100,
         pctfrpl = pctfrpl * 100)

p <- ggplot(df_long, aes(x = pctfrpl, y = pct_met, color = salary_label)) +
  geom_point(size = 1.6, alpha = 0.22, shape = 16) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.1) +
  facet_wrap(~ Subject, ncol = 2) +
  scale_color_manual(values = salary_quartile_colors, name = "Teacher salary quartile (2015 dollars)") +
  scale_x_continuous(labels = function(x) paste0(x, "%"), breaks = seq(0, 100, 20)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(
    title    = "Poverty rate vs. student test scores in Washington State",
    subtitle = "Each point = one district in one year, colored by salary quartile  |  Descriptive association, not a causal estimate",
    x        = "% students on free/reduced lunch (poverty proxy)",
    y        = "% of students meeting standard",
    caption  = NULL
  ) +
  theme_project(base_size = 13, legend_position = "top") +
  theme(
    legend.key.width = unit(1.2, "cm")
  )

ggsave(output_path,
       plot = p, width = 12, height = 6, dpi = 180, bg = "white")
cat("Viz 3 saved.\n")
