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
output_path <- file.path(output_dir, "viz3_poverty_vs_scores.png")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

df <- read.csv(data_path, stringsAsFactors = FALSE)

df_clean <- df %>%
  filter(!is.na(pctfrpl), !is.na(meantotalsalary), meantotalsalary > 0) %>%
  filter(!is.na(metMATH_pct) | !is.na(metELA_pct)) %>%
  filter(pctfrpl >= 0, pctfrpl <= 1)

# Salary quartile labels based on full dataset
df_clean <- df_clean %>%
  mutate(salary_quartile = ntile(meantotalsalary, 4),
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

salary_colors <- c("Q1 (Lowest pay)" = "#B5D4F4",
                   "Q2"              = "#378ADD",
                   "Q3"              = "#185FA5",
                   "Q4 (Highest pay)"= "#042C53")

p <- ggplot(df_long, aes(x = pctfrpl, y = pct_met, color = salary_label)) +
  geom_point(size = 1.6, alpha = 0.22, shape = 16) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.1) +
  facet_wrap(~ Subject, ncol = 2) +
  scale_color_manual(values = salary_colors, name = "Teacher salary quartile") +
  scale_x_continuous(labels = function(x) paste0(x, "%"), breaks = seq(0, 100, 20)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(
    title    = "Poverty rate vs. student test scores in Washington State",
    subtitle = "Each point = one district in one year, colored by teacher salary quartile",
    x        = "% students on free/reduced lunch (poverty proxy)",
    y        = "% of students meeting standard",
    caption  = "Source: Washington State district-level data"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, margin = margin(b = 4)),
    plot.subtitle    = element_text(color = "#5F5E5A", size = 10, margin = margin(b = 14)),
    plot.caption     = element_text(color = "#888780", size = 9, margin = margin(t = 10)),
    plot.margin      = margin(16, 20, 12, 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(color = "#5F5E5A", size = 10),
    axis.title       = element_text(color = "#5F5E5A", size = 10),
    strip.text       = element_text(face = "bold", size = 12),
    legend.position  = "top",
    legend.title     = element_text(size = 10, face = "bold"),
    legend.text      = element_text(size = 10),
    legend.key.width = unit(1.2, "cm")
  )

ggsave(output_path,
       plot = p, width = 12, height = 6, dpi = 180, bg = "white")
cat("Viz 3 saved.\n")
