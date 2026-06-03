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
output_path <- file.path(output_dir, "wa_salary_vs_scores_scatter.png")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

df <- read.csv(data_path, stringsAsFactors = FALSE)

# Filter to rows with valid salary and at least one test score
df_clean <- df %>%
  filter(!is.na(meantotalsalary), meantotalsalary > 0) %>%
  filter(!is.na(metMATH_pct) | !is.na(metELA_pct))

# Pivot to long: one row per district-year-subject
df_long <- df_clean %>%
  select(year, name, meantotalsalary, metMATH_pct, metELA_pct) %>%
  pivot_longer(
    cols = c(metMATH_pct, metELA_pct),
    names_to = "Subject",
    values_to = "pct_met"
  ) %>%
  filter(!is.na(pct_met), pct_met > 0) %>%
  mutate(
    Subject   = recode(Subject, metMATH_pct = "Math", metELA_pct = "ELA"),
    pct_met   = pct_met * 100,
    # Covid flag for visual callout
    covid_year = year == 2021
  )

p <- ggplot(df_long, aes(x = meantotalsalary, y = pct_met, color = Subject)) +

  # Covid year points highlighted with open ring behind them
  geom_point(
    data = df_long %>% filter(covid_year),
    aes(x = meantotalsalary, y = pct_met),
    shape = 21, size = 3.2, fill = NA,
    color = "#E24B4A", stroke = 0.8, alpha = 0.5
  ) +

  # Main scatter points
  geom_point(aes(alpha = ifelse(covid_year, 0.55, 0.25)), size = 1.8, shape = 16) +

  # Smooth trend line per subject
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.1, alpha = 0.12) +

  scale_alpha_identity() +

  scale_color_manual(values = c("Math" = "#378ADD", "ELA" = "#1D9E75")) +

  scale_x_continuous(
    labels = dollar_format(scale = 1e-3, suffix = "K"),
    breaks = seq(40000, 140000, by = 20000)
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)
  ) +

  # Facet by subject for clarity
  facet_wrap(~ Subject, ncol = 2) +

  labs(
    title    = "Teacher salaries vs. student test scores in Washington State",
    subtitle = "Each point = one district in one year  |  Line shows linear trend  |  Red rings = 2021 (Covid year)",
    x        = "Mean total teacher salary",
    y        = "% of students meeting standard",
    caption  = "Source: Washington State district-level data"
  ) +

  theme_minimal(base_size = 13) +
  theme(
    plot.title        = element_text(face = "bold", size = 15, margin = margin(b = 4)),
    plot.subtitle     = element_text(color = "#5F5E5A", size = 10, margin = margin(b = 14)),
    plot.caption      = element_text(color = "#888780", size = 9, margin = margin(t = 10)),
    plot.margin       = margin(16, 20, 12, 12),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    axis.text         = element_text(color = "#5F5E5A", size = 10),
    axis.title        = element_text(color = "#5F5E5A", size = 10),
    strip.text        = element_text(face = "bold", size = 12),
    legend.position   = "none"
  )

ggsave(
  output_path,
  plot   = p,
  width  = 12,
  height = 6,
  dpi    = 180,
  bg     = "white"
)

cat("Saved.\n")
