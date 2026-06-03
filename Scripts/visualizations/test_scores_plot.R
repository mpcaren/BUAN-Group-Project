library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Load data
data_path <- file.path(
  "Data",
  "converted",
  "1. Data for Student Performance Paper 2015 2025.csv"
)
output_dir <- file.path("Outputs", "figures")
output_path <- file.path(output_dir, "wa_test_scores_over_time.png")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

df <- read.csv(data_path, stringsAsFactors = FALSE)

# Keep only rows with valid test score data
df_scores <- df %>%
  filter(!is.na(metMATH_pct) & !is.na(metELA_pct)) %>%
  filter(metMATH_pct > 0 | metELA_pct > 0)

# Compute statewide average by year
yearly <- df_scores %>%
  group_by(year) %>%
  summarise(
    Math    = mean(metMATH_pct, na.rm = TRUE),
    ELA     = mean(metELA_pct,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(Math, ELA), names_to = "Subject", values_to = "pct_met") %>%
  mutate(pct_met = pct_met * 100)

# Covid shading bounds
covid_xmin <- 2019.5
covid_xmax <- 2021.5

# Peak pre-covid values for annotation
pre_covid <- yearly %>% filter(year <= 2019)
math_peak <- pre_covid %>% filter(Subject == "Math") %>% summarise(v = max(pct_met)) %>% pull(v)
ela_peak  <- pre_covid %>% filter(Subject == "ELA")  %>% summarise(v = max(pct_met)) %>% pull(v)

trough_math <- yearly %>% filter(Subject == "Math", year == 2021) %>% pull(pct_met)
trough_ela  <- yearly %>% filter(Subject == "ELA",  year == 2021) %>% pull(pct_met)

p <- ggplot(yearly, aes(x = year, y = pct_met, color = Subject, linetype = Subject)) +

  # Covid shading
  annotate("rect",
    xmin = covid_xmin, xmax = covid_xmax,
    ymin = -Inf, ymax = Inf,
    fill = "#E24B4A", alpha = 0.08
  ) +
  annotate("text",
    x = (covid_xmin + covid_xmax) / 2, y = 68,
    label = "Covid\ndisruption",
    size = 3, color = "#A32D2D", fontface = "italic", lineheight = 0.9
  ) +

  # Lines and points
  geom_line(linewidth = 1.1) +
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5) +

  # Labels at last year's point
  geom_text(
    data = yearly %>% group_by(Subject) %>% slice_max(year),
    aes(label = paste0(Subject, "\n", round(pct_met, 1), "%")),
    hjust = -0.15, size = 3.1, fontface = "bold", show.legend = FALSE
  ) +

  # Math low: text above, arrow pointing down to the data point
  annotate("text",
    x = 2021, y = trough_math + 10,
    label = paste0("Math low: ", round(trough_math, 1), "%"),
    size = 2.8, color = "#5F5E5A", lineheight = 0.9
  ) +
  annotate("segment",
    x = 2021, xend = 2021,
    y = trough_math + 7.5, yend = trough_math + 2,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = "#888780", linewidth = 0.5
  ) +

  # 2020 missing data annotation
  annotate("text",
    x = 2020, y = 23,
    label = "2020: no data\n(testing suspended)",
    size = 2.6, color = "#A32D2D", fontface = "italic", lineheight = 0.9
  ) +

  # Color and linetype scales
  scale_color_manual(
    values = c("Math" = "#378ADD", "ELA" = "#1D9E75")
  ) +
  scale_linetype_manual(
    values = c("Math" = "solid", "ELA" = "dashed")
  ) +

  # Axes
  scale_x_continuous(
    breaks = sort(unique(yearly$year)),
    expand = expansion(mult = c(0.03, 0.14))
  ) +
  scale_y_continuous(
    limits = c(20, 75),
    labels = function(x) paste0(x, "%"),
    breaks = seq(20, 70, by = 10)
  ) +

  # Labels
  labs(
    title    = "Student test scores over time in Washington State",
    subtitle = "Statewide average % of students meeting standard, by school year",
    x        = "School year",
    y        = "% meeting standard",
    color    = "Subject",
    linetype = "Subject",
    caption  = "Source: Washington State district-level data  |  Math and ELA % met standard averaged across all districts"
  ) +

  # Theme
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, margin = margin(b = 4)),
    plot.subtitle    = element_text(color = "#5F5E5A", size = 11, margin = margin(b = 14)),
    plot.caption     = element_text(color = "#888780", size = 9, margin = margin(t = 10)),
    plot.margin      = margin(16, 24, 12, 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(color = "#5F5E5A", size = 10),
    axis.title       = element_text(color = "#5F5E5A", size = 10),
    axis.text.x      = element_text(angle = 0, hjust = 0.5),
    legend.position  = "top",
    legend.justification = "left",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 11),
    legend.key.width = unit(1.8, "cm")
  )

ggsave(
  output_path,
  plot   = p,
  width  = 10,
  height = 6,
  dpi    = 180,
  bg     = "white"
)

cat("Plot saved.\n")
