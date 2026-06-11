required_packages <- c(
  "shiny",
  "leaflet",
  "sf",
  "dplyr",
  "htmltools",
  "scales",
  "viridisLite"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(htmltools)
library(scales)

find_project_root <- function(start_dir = getwd()) {
  current_dir <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)

  repeat {
    expected_data_path <- file.path(
      current_dir,
      "Data",
      "converted",
      "1. Data for Student Performance Paper 2015 2025.rds"
    )

    if (file.exists(expected_data_path)) {
      return(current_dir)
    }

    parent_dir <- dirname(current_dir)
    if (identical(parent_dir, current_dir)) {
      stop("Could not find the project root containing Data/converted.", call. = FALSE)
    }

    current_dir <- parent_dir
  }
}

project_root <- find_project_root()
data_path <- file.path(
  project_root,
  "Data",
  "converted",
  "1. Data for Student Performance Paper 2015 2025.rds"
)
figures_dir <- file.path(project_root, "Outputs", "figures")
boundary_dir <- file.path(project_root, "Data", "geo", "wa_unified_school_districts_2025")
boundary_zip <- file.path(boundary_dir, "tl_2025_53_unsd.zip")
boundary_url <- "https://www2.census.gov/geo/tiger/TIGER2025/UNSD/tl_2025_53_unsd.zip"

dir.create(boundary_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(boundary_zip)) {
  message("Downloading Washington unified school district boundaries...")
  download.file(boundary_url, boundary_zip, mode = "wb", quiet = FALSE)
}

if (length(list.files(boundary_dir, pattern = "\\.shp$", full.names = TRUE)) == 0) {
  unzip(boundary_zip, exdir = boundary_dir)
}

shape_path <- list.files(boundary_dir, pattern = "\\.shp$", full.names = TRUE)[1]

raw_data <- readRDS(data_path)

score_data <- raw_data |>
  mutate(
    nces_lea = as.character(nces_lea),
    math_score = metMATH_pct * 100,
    ela_score = metELA_pct * 100,
    spend_per_student = if_else(
      is.na(enrollment) | enrollment <= 0,
      NA_real_,
      tinstruction_cpi_adjusted / enrollment
    )
  ) |>
  group_by(year, nces_lea, name, county) |>
  summarize(
    math_score = mean(math_score, na.rm = TRUE),
    ela_score = mean(ela_score, na.rm = TRUE),
    expected_to_test = sum(expectedtotest_n, na.rm = TRUE),
    enrollment = mean(enrollment, na.rm = TRUE),
    pct_low_income = mean(pctfrpl, na.rm = TRUE) * 100,
    salary_cpi = mean(meantotalsalary_cpi_adjusted, na.rm = TRUE),
    spend_per_student = mean(spend_per_student, na.rm = TRUE),
    teacher_experience = mean(meanexperience, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(
    c(math_score, ela_score, enrollment, pct_low_income, salary_cpi,
      spend_per_student, teacher_experience),
    \(x) if_else(is.nan(x), NA_real_, x)
  )) |>
  mutate(
    both_score = if_else(
      is.na(math_score) | is.na(ela_score),
      NA_real_,
      (math_score + ela_score) / 2
    )
  )

district_shapes <- st_read(shape_path, quiet = TRUE) |>
  st_transform(4326) |>
  mutate(nces_lea = GEOID)

available_years <- sort(unique(score_data$year))
latest_year <- max(available_years)

# Pre-computed extents so map colors stay comparable across years.
score_values <- c(score_data$both_score, score_data$math_score, score_data$ela_score)
score_domain <- c(
  max(0, floor(min(score_values, na.rm = TRUE) / 5) * 5),
  min(100, ceiling(max(score_values, na.rm = TRUE) / 5) * 5)
)

median_distances <- score_data |>
  group_by(year) |>
  mutate(
    both_dist = both_score - median(both_score, na.rm = TRUE),
    math_dist = math_score - median(math_score, na.rm = TRUE),
    ela_dist = ela_score - median(ela_score, na.rm = TRUE)
  ) |>
  ungroup()

median_domain_max <- ceiling(
  max(abs(c(
    median_distances$both_dist,
    median_distances$math_dist,
    median_distances$ela_dist
  )), na.rm = TRUE) / 5
) * 5

# Bounding box per district so the search box can fly the map to a selection.
district_bounds <- do.call(rbind, lapply(seq_len(nrow(district_shapes)), function(i) {
  bb <- st_bbox(district_shapes$geometry[i])
  data.frame(
    nces_lea = district_shapes$nces_lea[i],
    xmin = bb[["xmin"]], ymin = bb[["ymin"]],
    xmax = bb[["xmax"]], ymax = bb[["ymax"]]
  )
}))

district_choices <- score_data |>
  distinct(nces_lea, name) |>
  filter(nces_lea %in% district_shapes$nces_lea, !is.na(name)) |>
  arrange(name)

search_choices <- setNames(district_choices$nces_lea, district_choices$name)

reference_cities <- data.frame(
  city = c(
    "Seattle", "Spokane", "Tacoma", "Vancouver", "Everett", "Bellingham",
    "Yakima", "Kennewick", "Wenatchee", "Olympia", "Walla Walla",
    "Port Angeles", "Moses Lake", "Pullman", "Aberdeen"
  ),
  lng = c(
    -122.3321, -117.4260, -122.4443, -122.6615, -122.2021, -122.4787,
    -120.5059, -119.1372, -120.3103, -122.9007, -118.3430,
    -123.4307, -119.2781, -117.1796, -123.8157
  ),
  lat = c(
    47.6062, 47.6588, 47.2529, 45.6387, 47.9790, 48.7519,
    46.6021, 46.2112, 47.4235, 47.0379, 46.0646,
    48.1181, 47.1301, 46.7298, 46.9754
  )
)

# Headline numbers for the Overview page, computed live from the data so they
# never drift out of sync with the underlying file.
latest_scores <- score_data |> filter(year == latest_year)
pre_covid_scores <- score_data |> filter(year == 2019)

kpi <- list(
  n_districts = n_distinct(latest_scores$nces_lea[!is.na(latest_scores$math_score) | !is.na(latest_scores$ela_score)]),
  math_median = median(latest_scores$math_score, na.rm = TRUE),
  ela_median = median(latest_scores$ela_score, na.rm = TRUE),
  math_delta = median(latest_scores$math_score, na.rm = TRUE) - median(pre_covid_scores$math_score, na.rm = TRUE),
  ela_delta = median(latest_scores$ela_score, na.rm = TRUE) - median(pre_covid_scores$ela_score, na.rm = TRUE),
  students = sum(latest_scores$expected_to_test, na.rm = TRUE)
)

addResourcePath("figures", figures_dir)

figure_path <- function(filename) {
  paste0("figures/", URLencode(filename, reserved = TRUE))
}

result_figure <- function(filename, alt) {
  tags$a(
    class = "figure-link",
    href = figure_path(filename),
    target = "_blank",
    title = "Open full-size chart",
    tags$img(
      class = "result-figure",
      src = figure_path(filename),
      alt = alt
    )
  )
}

chart_card <- function(title, filename, alt, class = "") {
  div(
    class = paste("chart-card", class),
    h3(title),
    result_figure(filename, alt)
  )
}

finding_heading <- function(text) {
  h2(class = "finding-heading", text)
}

kpi_card <- function(value, label, note = NULL, tone = "neutral") {
  div(
    class = paste("kpi-card", paste0("kpi-", tone)),
    div(class = "kpi-value", value),
    div(class = "kpi-label", label),
    if (!is.null(note)) div(class = "kpi-note", note)
  )
}

step_card <- function(number, title, description, target) {
  div(
    class = "step-card",
    onclick = sprintf("Shiny.setInputValue('go_tab', '%s', {priority: 'event'})", target),
    div(class = "step-number", number),
    div(
      class = "step-body",
      h2(title),
      p(description)
    ),
    div(class = "step-arrow", HTML("&rarr;"))
  )
}

page_shell <- function(title, subtitle, ...) {
  div(
    class = "dashboard-shell",
    div(
      class = "dashboard-intro",
      h1(title),
      p(subtitle)
    ),
    ...
  )
}

dashboard_css <- "
  :root {
    --ink: #22313f;
    --ink-soft: #5d6b78;
    --ink-faint: #8b97a3;
    --navy: #1f3a52;
    --teal: #2a9d8f;
    --teal-dark: #1f7a70;
    --teal-soft: #e7f4f2;
    --paper: #f5f7f9;
    --card: #ffffff;
    --line: #dde4ea;
    --red: #d65f5f;
    --shadow: 0 1px 2px rgba(20, 45, 70, 0.06), 0 10px 28px rgba(20, 45, 70, 0.07);
  }

  body {
    color: var(--ink);
    background: var(--paper);
    font-family: 'Inter', 'Segoe UI', system-ui, -apple-system, sans-serif;
    -webkit-font-smoothing: antialiased;
  }

  .navbar {
    margin-bottom: 0;
    border: 0;
    border-radius: 0;
    background: linear-gradient(120deg, #18374f 0%, #1f4a60 70%, #1f5d63 100%);
    box-shadow: 0 2px 10px rgba(15, 35, 55, 0.25);
  }

  .navbar-default .navbar-brand {
    color: #ffffff;
    font-weight: 700;
    letter-spacing: 0.2px;
  }

  .navbar-default .navbar-nav > li > a {
    color: #cfe0e6;
    font-weight: 500;
  }

  .navbar-default .navbar-nav > .active > a,
  .navbar-default .navbar-nav > .active > a:hover,
  .navbar-default .navbar-nav > .active > a:focus {
    color: #ffffff;
    background: rgba(255, 255, 255, 0.14);
    box-shadow: inset 0 -3px 0 var(--teal);
  }

  .navbar-default .navbar-nav > li > a:hover {
    color: #ffffff;
    background: rgba(255, 255, 255, 0.08);
  }

  .dashboard-shell {
    max-width: 1500px;
    margin: 0 auto;
    padding: 30px 28px 56px;
  }

  .dashboard-intro {
    padding: 6px 0 22px;
  }

  .dashboard-intro h1 {
    margin: 0 0 8px;
    font-size: 30px;
    font-weight: 750;
    color: var(--navy);
    letter-spacing: -0.3px;
  }

  .dashboard-intro p {
    max-width: 940px;
    margin: 0;
    color: var(--ink-soft);
    font-size: 16px;
    line-height: 1.6;
  }

  /* ---- Overview ---- */

  .kpi-grid {
    display: grid;
    grid-template-columns: repeat(4, minmax(0, 1fr));
    gap: 16px;
    margin: 6px 0 30px;
  }

  .kpi-card {
    padding: 18px 20px 16px;
    background: var(--card);
    border: 1px solid var(--line);
    border-radius: 12px;
    box-shadow: var(--shadow);
  }

  .kpi-value {
    font-size: 30px;
    font-weight: 750;
    color: var(--navy);
    letter-spacing: -0.5px;
    line-height: 1.1;
  }

  .kpi-card.kpi-teal .kpi-value { color: var(--teal-dark); }

  .kpi-label {
    margin-top: 6px;
    font-size: 13.5px;
    font-weight: 600;
    color: var(--ink-soft);
  }

  .kpi-note {
    margin-top: 4px;
    font-size: 12.5px;
    color: var(--ink-faint);
  }

  .step-list {
    display: grid;
    gap: 14px;
  }

  .step-card {
    display: flex;
    gap: 18px;
    align-items: center;
    padding: 18px 22px;
    background: var(--card);
    border: 1px solid var(--line);
    border-radius: 12px;
    box-shadow: var(--shadow);
    cursor: pointer;
    transition: transform 0.15s ease, border-color 0.15s ease;
  }

  .step-card:hover {
    transform: translateY(-2px);
    border-color: var(--teal);
  }

  .step-card .step-number {
    display: inline-flex;
    width: 38px;
    height: 38px;
    align-items: center;
    justify-content: center;
    flex: 0 0 38px;
    border-radius: 50%;
    background: var(--teal);
    color: white;
    font-weight: 700;
    font-size: 16px;
  }

  .step-card h2 {
    margin: 0 0 3px;
    font-size: 18px;
    font-weight: 700;
    color: var(--navy);
  }

  .step-card p {
    margin: 0;
    color: var(--ink-soft);
    font-size: 14.5px;
  }

  .step-card .step-body { flex: 1 1 auto; }

  .step-card .step-arrow {
    font-size: 22px;
    color: var(--ink-faint);
    transition: transform 0.15s ease, color 0.15s ease;
  }

  .step-card:hover .step-arrow {
    transform: translateX(4px);
    color: var(--teal);
  }

  .page-footnote {
    margin-top: 26px;
    font-size: 12.5px;
    color: var(--ink-faint);
  }

  .finding-heading {
    margin: 0 0 16px;
    color: var(--teal-dark);
    font-size: 18px;
    font-weight: 750;
    line-height: 1.35;
  }

  /* ---- Chart cards ---- */

  .chart-grid {
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 18px;
  }

  .chart-grid.single {
    grid-template-columns: minmax(0, 1fr);
  }

  .chart-card {
    min-width: 0;
    display: flex;
    flex-direction: column;
    padding: 18px 18px 16px;
    border: 1px solid var(--line);
    border-radius: 12px;
    background: var(--card);
    box-shadow: var(--shadow);
    transition: transform 0.15s ease;
  }

  .chart-card:hover { transform: translateY(-2px); }

  .chart-card h3 {
    margin: 0 0 12px;
    font-size: 16px;
    font-weight: 700;
    color: var(--navy);
  }

  .result-figure {
    display: block;
    width: 100%;
    height: auto;
    max-height: 660px;
    object-fit: contain;
    border-radius: 6px;
  }

  .figure-link {
    display: block;
    border-radius: 6px;
  }

  .figure-link:focus-visible {
    outline: 3px solid rgba(42, 157, 143, 0.35);
    outline-offset: 3px;
  }

  /* ---- Chart browser ---- */

  .chart-browser {
    max-width: 1450px;
    margin: 0 auto;
    padding: 28px 28px 48px;
  }

  .chart-browser h2 {
    margin: 0 0 6px;
    font-size: 26px;
    font-weight: 750;
    color: var(--navy);
  }

  .chart-browser .browser-sub {
    margin: 0 0 18px;
    color: var(--ink-soft);
  }

  .chart-browser .form-group {
    max-width: 480px;
  }

  .chart-browser-frame {
    padding: 18px;
    border: 1px solid var(--line);
    border-radius: 12px;
    background: var(--card);
    box-shadow: var(--shadow);
  }

  .browser-caption {
    margin: 14px 4px 0;
    font-size: 14px;
    color: var(--ink-soft);
    line-height: 1.55;
  }

  /* ---- Map page ---- */

  .map-page {
    max-width: 1600px;
    margin: 0 auto;
    padding: 24px 26px 40px;
  }

  .map-header {
    max-width: 1150px;
    margin-bottom: 16px;
  }

  .map-header h2 {
    margin: 0 0 6px;
    color: var(--navy);
    font-size: 26px;
    font-weight: 750;
    letter-spacing: -0.3px;
  }

  .map-header p {
    margin: 0;
    color: var(--ink-soft);
    line-height: 1.55;
  }

  .map-controls {
    display: flex;
    flex-wrap: wrap;
    gap: 26px;
    align-items: flex-end;
    padding: 16px 20px;
    border: 1px solid var(--line);
    border-bottom: 0;
    border-radius: 12px 12px 0 0;
    background: var(--card);
  }

  .map-controls .form-group,
  .map-controls .shiny-input-container {
    margin-bottom: 0;
  }

  .map-controls .control-label {
    font-size: 12px;
    font-weight: 700;
    letter-spacing: 0.5px;
    text-transform: uppercase;
    color: var(--ink-faint);
  }

  .subject-control { flex: 0 0 auto; }
  .metric-control { flex: 0 0 auto; }

  .year-control {
    flex: 1 1 260px;
    min-width: 240px;
  }

  .year-control .shiny-input-container {
    width: 100%;
    max-width: 640px;
  }

  .search-control { flex: 0 0 260px; }

  .search-control .selectize-input {
    border-radius: 8px;
    border-color: var(--line);
    box-shadow: none;
  }

  .reset-control { flex: 0 0 auto; }

  .reset-control .btn {
    border-radius: 8px;
    border: 1px solid var(--line);
    background: #f2f5f7;
    color: var(--navy);
    font-weight: 600;
  }

  .reset-control .btn:hover {
    background: var(--teal-soft);
    border-color: var(--teal);
  }

  .cities-control {
    flex: 0 0 auto;
    padding-bottom: 6px;
  }

  .cities-control .checkbox { margin: 0; }

  .cities-control input[type='checkbox'] {
    accent-color: var(--teal);
  }

  .cities-control label {
    font-weight: 600;
    color: var(--ink-soft);
    font-size: 13.5px;
  }

  /* Segmented pill look for inline radio groups on the map page */
  .pill-group .shiny-options-group {
    display: inline-flex;
    padding: 3px;
    background: #eef2f5;
    border-radius: 999px;
  }

  .pill-group .radio-inline {
    position: relative;
    margin: 0;
    padding: 5px 16px;
    border-radius: 999px;
    font-weight: 600;
    font-size: 13.5px;
    color: var(--ink-soft);
    cursor: pointer;
    transition: background 0.12s ease, color 0.12s ease;
  }

  .pill-group .radio-inline + .radio-inline { margin-left: 2px; }

  .pill-group .radio-inline input {
    position: absolute;
    opacity: 0;
    pointer-events: none;
  }

  .pill-group .radio-inline:has(input:checked) {
    background: var(--card);
    color: var(--teal-dark);
    box-shadow: 0 1px 3px rgba(20, 45, 70, 0.18);
  }

  /* Brand the year slider */
  .irs--shiny .irs-bar,
  .irs--shiny .irs-single,
  .irs--shiny .irs-from,
  .irs--shiny .irs-to {
    background: var(--teal);
    border-color: var(--teal);
  }

  .irs--shiny .irs-handle { border-color: var(--teal); }

  .map-statbar {
    display: grid;
    grid-template-columns: repeat(4, minmax(0, 1fr));
    border: 1px solid var(--line);
    border-bottom: 0;
    background: #fbfcfd;
  }

  .map-stat {
    padding: 10px 18px;
    border-right: 1px solid var(--line);
  }

  .map-stat:last-child { border-right: 0; }

  .map-stat .stat-label {
    font-size: 11px;
    font-weight: 700;
    letter-spacing: 0.5px;
    text-transform: uppercase;
    color: var(--ink-faint);
  }

  .map-stat .stat-value {
    font-size: 17px;
    font-weight: 700;
    color: var(--navy);
  }

  .map-stat .stat-detail {
    font-size: 12px;
    color: var(--ink-soft);
  }

  .map-frame {
    overflow: hidden;
    border: 1px solid var(--line);
    border-radius: 0 0 12px 12px;
    background: var(--card);
    box-shadow: var(--shadow);
  }

  .map-hint {
    margin-top: 12px;
    font-size: 13px;
    color: var(--ink-faint);
  }

  .map-hint .fa { margin-right: 5px; color: var(--teal-dark); }

  .city-label {
    background: transparent;
    border: 0;
    box-shadow: none;
    color: #44525e;
    font-weight: 700;
    font-size: 11.5px;
    text-shadow: 0 0 3px #ffffff, 0 0 6px #ffffff, 0 0 9px #ffffff;
  }

  .leaflet-popup-content-wrapper {
    border-radius: 10px;
    box-shadow: 0 8px 28px rgba(15, 35, 55, 0.25);
  }

  .district-popup { font-family: inherit; }

  .district-popup h4 {
    margin: 2px 0 1px;
    font-size: 15.5px;
    font-weight: 750;
    color: var(--navy);
  }

  .district-popup .popup-county {
    margin: 0 0 8px;
    font-size: 12px;
    color: var(--ink-faint);
  }

  .district-popup table { width: 100%; border-collapse: collapse; }

  .district-popup td {
    padding: 3px 0;
    font-size: 12.5px;
    color: var(--ink-soft);
  }

  .district-popup td.popup-num {
    text-align: right;
    font-weight: 700;
    color: var(--navy);
    white-space: nowrap;
  }

  .district-popup .popup-section {
    margin: 8px 0 3px;
    font-size: 10.5px;
    font-weight: 700;
    letter-spacing: 0.6px;
    text-transform: uppercase;
    color: var(--ink-faint);
    border-top: 1px solid var(--line);
    padding-top: 7px;
  }

  .district-popup .popup-spark { margin-top: 6px; }

  .district-popup .spark-caption {
    font-size: 11px;
    color: var(--ink-faint);
    margin-top: 2px;
  }

  @media (max-width: 1100px) {
    .kpi-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
    .map-statbar { grid-template-columns: repeat(2, minmax(0, 1fr)); }
    .map-stat { border-bottom: 1px solid var(--line); }
  }

  @media (max-width: 900px) {
    .dashboard-shell,
    .chart-browser,
    .map-page {
      padding-left: 14px;
      padding-right: 14px;
    }

    .chart-grid { grid-template-columns: minmax(0, 1fr); }

    .map-controls { display: block; }

    .map-controls > div { margin-bottom: 12px; }

    .kpi-grid { grid-template-columns: minmax(0, 1fr); }
  }

  @media (max-width: 600px) {
    body {
      overflow-x: hidden;
      font-size: 14px;
    }

    .navbar-header {
      min-height: 52px;
    }

    .navbar-default .navbar-brand {
      max-width: calc(100vw - 72px);
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
      font-size: 16px;
      line-height: 22px;
    }

    .navbar-default .navbar-toggle {
      margin-top: 9px;
      margin-bottom: 9px;
      border-color: rgba(255, 255, 255, 0.35);
    }

    .navbar-default .navbar-toggle .icon-bar {
      background-color: #ffffff;
    }

    .navbar-default .navbar-collapse {
      border-color: rgba(255, 255, 255, 0.14);
      box-shadow: none;
    }

    .navbar-default .navbar-nav {
      margin-top: 4px;
      margin-bottom: 8px;
    }

    .navbar-default .navbar-nav > li > a {
      min-height: 44px;
      padding-top: 12px;
      padding-bottom: 12px;
    }

    .navbar-default .navbar-nav > .active > a,
    .navbar-default .navbar-nav > .active > a:hover,
    .navbar-default .navbar-nav > .active > a:focus {
      box-shadow: inset 3px 0 0 var(--teal);
    }

    .dashboard-shell,
    .chart-browser,
    .map-page {
      padding: 18px 10px 34px;
    }

    .dashboard-intro h1,
    .chart-browser h2,
    .map-header h2 {
      font-size: 23px;
      line-height: 1.18;
    }

    .dashboard-intro {
      padding-bottom: 16px;
    }

    .dashboard-intro p,
    .map-header p,
    .chart-browser .browser-sub {
      font-size: 14px;
      line-height: 1.5;
    }

    .kpi-grid {
      grid-template-columns: repeat(2, minmax(0, 1fr));
      gap: 8px;
      margin-bottom: 18px;
    }

    .kpi-card {
      min-width: 0;
      padding: 13px 12px;
      border-radius: 10px;
    }

    .kpi-value {
      font-size: 23px;
    }

    .kpi-label {
      margin-top: 5px;
      font-size: 12px;
      line-height: 1.3;
    }

    .kpi-note {
      font-size: 11px;
      line-height: 1.3;
    }

    .step-list {
      gap: 9px;
    }

    .step-card {
      gap: 11px;
      padding: 13px 12px;
      border-radius: 10px;
    }

    .step-card .step-number {
      width: 32px;
      height: 32px;
      flex-basis: 32px;
      font-size: 13px;
    }

    .step-card h2 {
      font-size: 15px;
    }

    .step-card p {
      font-size: 12.5px;
      line-height: 1.4;
    }

    .step-card .step-arrow {
      font-size: 18px;
    }

    .page-footnote {
      margin-top: 18px;
      font-size: 11.5px;
      line-height: 1.45;
    }

    .finding-heading {
      margin-bottom: 12px;
      font-size: 16px;
    }

    .chart-grid {
      gap: 12px;
    }

    .chart-card {
      padding: 12px;
      border-radius: 10px;
    }

    .chart-card:hover,
    .step-card:hover {
      transform: none;
    }

    .chart-card h3 {
      margin-bottom: 8px;
      font-size: 15px;
    }

    .result-figure {
      max-height: none;
    }

    .browser-caption {
      font-size: 12.5px;
      line-height: 1.5;
    }

    .chart-browser .form-group {
      width: 100%;
      max-width: none;
    }

    .chart-browser-frame {
      padding: 10px;
      border-radius: 10px;
    }

    .map-header {
      margin-bottom: 12px;
    }

    .map-controls {
      display: grid;
      grid-template-columns: repeat(2, minmax(0, 1fr));
      gap: 0 12px;
      padding: 12px;
      border-radius: 10px 10px 0 0;
    }

    .map-controls > div {
      margin-bottom: 10px;
    }

    .subject-control,
    .metric-control,
    .year-control,
    .search-control {
      grid-column: 1 / -1;
    }

    .map-controls > .cities-control,
    .map-controls > .reset-control {
      margin-bottom: 0;
    }

    .map-controls > .cities-control {
      justify-self: start;
    }

    .map-controls > .reset-control {
      justify-self: stretch;
    }

    .map-controls > div:last-child {
      margin-bottom: 0;
    }

    .map-controls .control-label {
      margin-bottom: 5px;
    }

    .map-statbar {
      display: flex;
      gap: 0;
      overflow-x: auto;
      overscroll-behavior-x: contain;
      scrollbar-width: thin;
      scroll-snap-type: x proximity;
      -webkit-overflow-scrolling: touch;
    }

    .map-stat {
      flex: 0 0 47%;
      min-width: 145px;
      padding: 9px 12px;
      border-right: 1px solid var(--line);
      border-bottom: 0;
      scroll-snap-align: start;
    }

    .map-stat .stat-label {
      font-size: 9.5px;
      line-height: 1.25;
    }

    .map-stat .stat-value {
      font-size: 15px;
    }

    .map-stat .stat-detail {
      overflow: hidden;
      font-size: 10.5px;
      text-overflow: ellipsis;
      white-space: nowrap;
    }

    .subject-control,
    .metric-control,
    .year-control,
    .search-control,
    .year-control .shiny-input-container,
    .search-control .shiny-input-container {
      width: 100%;
      min-width: 0;
      max-width: none;
    }

    .pill-group .shiny-options-group {
      display: flex;
      width: 100%;
    }

    .pill-group .radio-inline {
      flex: 1 1 auto;
      min-height: 44px;
      padding: 11px 10px;
      text-align: center;
    }

    .search-control .selectize-input {
      min-height: 44px;
      padding-top: 10px;
      padding-bottom: 8px;
    }

    .cities-control {
      display: inline-flex;
      align-items: center;
      min-height: 44px;
      padding-bottom: 0;
    }

    .cities-control label {
      padding: 10px 0;
    }

    .reset-control .btn {
      min-height: 44px;
      width: 100%;
    }

    .map-frame {
      border-radius: 0 0 10px 10px;
    }

    #explore_map,
    .map-frame .leaflet {
      height: 62vh !important;
      min-height: 430px;
      max-height: 590px;
    }

    .leaflet-control-zoom a {
      width: 36px;
      height: 36px;
      line-height: 36px;
    }

    .leaflet-control-layers,
    .leaflet-control-scale {
      font-size: 10px;
    }

    .leaflet-control-attribution {
      max-width: 65vw;
      overflow: hidden;
      font-size: 8px;
      text-overflow: ellipsis;
      white-space: nowrap;
    }

    .leaflet-control .legend {
      max-width: 145px;
      padding: 5px 7px;
      font-size: 10px;
      line-height: 14px;
    }

    .leaflet-popup-content-wrapper {
      max-height: 70vh;
      overflow-y: auto;
      border-radius: 9px;
    }

    .leaflet-popup-content {
      width: min(270px, calc(100vw - 72px)) !important;
      margin: 10px 12px;
    }

    .district-popup h4 {
      padding-right: 14px;
      font-size: 14px;
    }

    .district-popup td {
      padding: 3px 0;
      font-size: 11.5px;
    }

    .district-popup .popup-spark svg {
      max-width: 100%;
      height: auto;
    }

    .city-label {
      font-size: 9.5px;
    }

    .map-hint {
      margin-top: 9px;
      font-size: 11.5px;
      line-height: 1.45;
    }
  }
"

dashboard_js <- "
  $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function() {
    if (window.matchMedia('(max-width: 767px)').matches) {
      $('.navbar-collapse.in').collapse('hide');
    }

    setTimeout(function() {
      var widget = HTMLWidgets.find('#explore_map');
      if (widget && widget.getMap) {
        widget.getMap().invalidateSize();
      }
      $(window).trigger('resize');
    }, 300);
  });
"

chart_choices <- c(
  "Test scores over time" = "wa_test_scores_over_time.png",
  "Salary and student outcomes (all years)" = "wa_salary_vs_scores_scatter.png",
  "Spending and student outcomes" = "viz_spend_final.png",
  "Spending by poverty quartile" = "viz_spend_by_poverty.png",
  "Student demographics by district type" = "viz4_demo_comparison.png",
  "Pay gap and learning gap" = "viz4_wealth_poverty_profiles.png",
  "Average teacher experience over time" = "mean_teacher_experience_timeseries_report_card.png"
)

chart_captions <- c(
  "wa_test_scores_over_time.png" = "Statewide shares of students meeting the Math and ELA standard each school year. Both lines are substantially lower after the 2020 testing pause and have recovered only partially.",
  "wa_salary_vs_scores_scatter.png" = "Each dot is one district in one year. The fitted line shows a slight negative association between teacher salary and results; it does not estimate the effect of salary.",
  "viz_spend_final.png" = "Each dot is one district in one year. Higher instructional spending per student is not associated with a higher share of students meeting standard.",
  "viz_spend_by_poverty.png" = "Average instructional spending per student by district poverty quartile. Higher-poverty districts actually spend somewhat more per student than wealthier ones.",
  "viz4_demo_comparison.png" = "Who attends high-poverty versus high-wealth districts: high-poverty districts enroll far more Hispanic and English-as-a-second-language students.",
  "viz4_wealth_poverty_profiles.png" = "Teacher pay differs by roughly 9% between high-wealth and high-poverty districts, but the gap in students meeting standard is around 20 percentage points.",
  "mean_teacher_experience_timeseries_report_card.png" = "Statewide average teacher experience over time from report-card data."
)

format_pct <- function(x, acc = 0.1) {
  if (is.na(x)) "—" else paste0(number(x, accuracy = acc), "%")
}

format_pts <- function(x) {
  if (is.na(x)) "—" else paste0(number(x, accuracy = 0.1, style_positive = "plus"), " pts")
}

format_dollars <- function(x) {
  if (is.na(x)) "—" else dollar(x, accuracy = 1)
}

# Tiny inline SVG line chart used inside map popups: one line per subject
# across all available years, with the Covid testing gap left visible.
sparkline_svg <- function(years, math, ela, width = 268, height = 84) {
  pad_l <- 30
  pad_r <- 12
  pad_t <- 10
  pad_b <- 16

  vals <- c(math, ela)
  if (all(is.na(vals))) {
    return("")
  }

  y_min <- max(0, floor(min(vals, na.rm = TRUE) / 10) * 10)
  y_max <- min(100, ceiling(max(vals, na.rm = TRUE) / 10) * 10)
  if (y_max - y_min < 10) y_max <- min(100, y_min + 10)

  x_scale <- function(yr) {
    pad_l + (yr - min(years)) / (max(years) - min(years)) * (width - pad_l - pad_r)
  }
  y_scale <- function(v) {
    height - pad_b - (v - y_min) / (y_max - y_min) * (height - pad_t - pad_b)
  }

  line_path <- function(values) {
    keep <- !is.na(values)
    if (sum(keep) < 2) return("")
    pts <- paste0(
      round(x_scale(years[keep]), 1), ",", round(y_scale(values[keep]), 1),
      collapse = " "
    )
    pts
  }

  dots <- function(values, color) {
    keep <- which(!is.na(values))
    paste0(vapply(keep, function(i) {
      sprintf(
        "<circle cx='%.1f' cy='%.1f' r='2.1' fill='%s'/>",
        x_scale(years[i]), y_scale(values[i]), color
      )
    }, character(1)), collapse = "")
  }

  end_label <- function(values, color) {
    keep <- which(!is.na(values))
    if (length(keep) == 0) return("")
    i <- max(keep)
    sprintf(
      "<text x='%.1f' y='%.1f' font-size='9' font-weight='700' fill='%s'>%s</text>",
      x_scale(years[i]) - 16, y_scale(values[i]) - 5, color,
      paste0(round(values[i]), "%")
    )
  }

  math_color <- "#2a7f8f"
  ela_color <- "#2a9d8f"

  paste0(
    sprintf("<svg width='%d' height='%d' viewBox='0 0 %d %d' xmlns='http://www.w3.org/2000/svg'>", width, height, width, height),
    sprintf("<line x1='%d' y1='%.1f' x2='%d' y2='%.1f' stroke='#dde4ea' stroke-width='1'/>", pad_l, y_scale(y_min), width - pad_r, y_scale(y_min)),
    sprintf("<line x1='%d' y1='%.1f' x2='%d' y2='%.1f' stroke='#eef2f5' stroke-width='1'/>", pad_l, y_scale(y_max), width - pad_r, y_scale(y_max)),
    sprintf("<text x='%d' y='%.1f' font-size='9' fill='#8b97a3'>%d%%</text>", 4, y_scale(y_min) + 3, y_min),
    sprintf("<text x='%d' y='%.1f' font-size='9' fill='#8b97a3'>%d%%</text>", 4, y_scale(y_max) + 3, y_max),
    sprintf("<text x='%.1f' y='%d' font-size='9' fill='#8b97a3'>%d</text>", x_scale(min(years)) - 8, height - 4, min(years)),
    sprintf("<text x='%.1f' y='%d' font-size='9' fill='#8b97a3'>%d</text>", x_scale(max(years)) - 14, height - 4, max(years)),
    if (nzchar(line_path(ela))) sprintf("<polyline points='%s' fill='none' stroke='%s' stroke-width='1.8' stroke-dasharray='4 3'/>", line_path(ela), ela_color) else "",
    if (nzchar(line_path(math))) sprintf("<polyline points='%s' fill='none' stroke='%s' stroke-width='1.8'/>", line_path(math), math_color) else "",
    dots(ela, ela_color),
    dots(math, math_color),
    end_label(math, math_color),
    "</svg>"
  )
}

ui <- navbarPage(
  title = "Washington Education Results",
  id = "main_navigation",
  collapsible = TRUE,
  header = tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap"
    ),
    tags$style(HTML(dashboard_css)),
    tags$script(HTML(dashboard_js))
  ),
  tabPanel(
    "Overview",
    value = "overview",
    page_shell(
      "Washington district outcomes, resources, and equity",
      paste(
        "Washington district-level results from 2015 to 2025:",
        "student performance, resources, and equity.",
        "Dollar values are CPI-adjusted to 2015."
      ),
      div(
        class = "kpi-grid",
        kpi_card(
          format_pct(kpi$math_median),
          sprintf("Median district Math rate, %d", latest_year),
          sprintf("%s vs. 2019 (pre-Covid)", format_pts(kpi$math_delta)),
          tone = "teal"
        ),
        kpi_card(
          format_pct(kpi$ela_median),
          sprintf("Median district ELA rate, %d", latest_year),
          sprintf("%s vs. 2019 (pre-Covid)", format_pts(kpi$ela_delta)),
          tone = "teal"
        ),
        kpi_card(
          comma(kpi$n_districts),
          "Districts reporting",
          sprintf("Statewide, %d", latest_year)
        ),
        kpi_card(
          comma(kpi$students),
          "Students expected to test",
          sprintf("Statewide, %d", latest_year)
        )
      ),
      div(
        class = "step-list",
        step_card(
          "1", "Outcomes",
          "Math and ELA performance over time.",
          "outcomes"
        ),
        step_card(
          "2", "Resources",
          "Teacher pay and spending compared with results.",
          "resources"
        ),
        step_card(
          "3", "Equity",
          "Resources and results across district poverty levels.",
          "equity"
        ),
        step_card(
          "4", "Map Explorer",
          "Explore district pass rates and profiles.",
          "map"
        )
      ),
      div(
        class = "page-footnote",
        "Rates shown are the share of students meeting the state exam standard. ",
        "Source: Washington State district-level data, 2015–2025. 2020 testing was suspended statewide. ",
        "All comparisons are observational and descriptive; they do not establish causal effects."
      )
    )
  ),
  tabPanel(
    "Outcomes",
    value = "outcomes",
    page_shell(
      "Student outcomes",
      "Statewide Math and ELA performance across the study period.",
      finding_heading("Scores are lower after the 2020 testing pause and have not fully recovered."),
      div(
        class = "chart-grid single",
        chart_card(
          "Statewide test-score trend",
          "wa_test_scores_over_time.png",
          "Line chart of statewide Math and ELA scores over time"
        )
      )
    )
  ),
  tabPanel(
    "Resources",
    value = "resources",
    page_shell(
      "Resources and student outcomes",
      "Teacher salary and instructional spending compared with student results. Dollar values are CPI-adjusted to 2015.",
      finding_heading("Spending and scores have a slight negative association in these district-level data."),
      div(
        class = "chart-grid",
        chart_card(
          "Teacher salaries and outcomes",
          "wa_salary_vs_scores_scatter.png",
          "Scatter plot comparing teacher salary with composite student outcomes"
        ),
        chart_card(
          "Instructional spending and outcomes",
          "viz_spend_final.png",
          "Scatter plot comparing instructional spending per student with composite outcomes"
        )
      )
    )
  ),
  tabPanel(
    "Equity",
    value = "equity",
    page_shell(
      "Distribution and equity",
      "Resources, teachers, demographics, and outcomes compared across district poverty levels.",
      finding_heading("The largest observed score differences are across district poverty levels."),
      div(
        class = "chart-grid",
        chart_card(
          "Spending by poverty quartile",
          "viz_spend_by_poverty.png",
          "Line chart of instructional spending by poverty quartile"
        ),
        chart_card(
          "Student demographics",
          "viz4_demo_comparison.png",
          "Bar chart comparing student demographics in high-wealth and high-poverty districts"
        ),
        chart_card(
          "Pay gap and learning gap",
          "viz4_wealth_poverty_profiles.png",
          "Bar chart comparing salaries and test scores in high-wealth and high-poverty districts"
        )
      )
    )
  ),
  tabPanel(
    "Map Explorer",
    value = "map",
    div(
      class = "map-page",
      div(
        class = "map-header",
        h2("Explore districts on the map"),
        p(paste(
          "Explore district pass rates or distance from the statewide median.",
          "The combined rate averages Math and ELA. Click a district for its profile and trend."
        ))
      ),
      div(
        class = "map-controls",
        div(
          class = "subject-control pill-group",
          radioButtons(
            inputId = "map_subject",
            label = "Subject",
            choices = c(
              "Both" = "both_score",
              "Math" = "math_score",
              "ELA" = "ela_score"
            ),
            selected = "both_score",
            inline = TRUE
          )
        ),
        div(
          class = "metric-control pill-group",
          radioButtons(
            inputId = "map_metric",
            label = "Color districts by",
            choices = c("Pass rate" = "rate", "Vs. state median" = "median"),
            selected = "rate",
            inline = TRUE
          )
        ),
        div(
          class = "year-control",
          sliderInput(
            inputId = "map_year",
            label = "Year",
            min = min(available_years),
            max = max(available_years),
            value = latest_year,
            step = 1,
            sep = "",
            ticks = TRUE
          )
        ),
        div(
          class = "search-control",
          selectizeInput(
            inputId = "map_search",
            label = "Find a district",
            choices = c("Type a district name..." = "", search_choices),
            selected = ""
          )
        ),
        div(
          class = "cities-control",
          checkboxInput("map_cities", "City labels", value = TRUE)
        ),
        div(
          class = "reset-control",
          actionButton("map_reset", label = tagList(icon("expand"), "Reset view"))
        )
      ),
      uiOutput("map_statbar"),
      div(class = "map-frame", leafletOutput("explore_map", height = "720px")),
      div(
        class = "map-hint",
        icon("hand-pointer"),
        paste(
          "Gray districts have no reported data for the selected measure and year.",
          "The combined rate averages Math and ELA."
        )
      )
    )
  ),
  tabPanel(
    "All Charts",
    value = "charts",
    div(
      class = "chart-browser",
      h2("Browse full-size results"),
      p(
        class = "browser-sub",
        "Every figure produced for the project, full size, with a short plain-English caption."
      ),
      selectInput("selected_chart", "Figure", choices = chart_choices),
      div(
        class = "chart-browser-frame",
        uiOutput("selected_chart_output"),
        uiOutput("selected_chart_caption")
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$go_tab, {
    updateNavbarPage(session, "main_navigation", selected = input$go_tab)
  })

  output$selected_chart_output <- renderUI({
    req(input$selected_chart)
    result_figure(input$selected_chart, names(chart_choices)[chart_choices == input$selected_chart])
  })

  output$selected_chart_caption <- renderUI({
    req(input$selected_chart)
    caption <- chart_captions[[input$selected_chart]]
    if (is.null(caption)) return(NULL)
    p(class = "browser-caption", caption)
  })

  subject_label <- reactive({
    if (identical(input$map_subject, "both_score")) {
      "Combined Math/ELA"
    } else if (identical(input$map_subject, "ela_score")) {
      "ELA"
    } else {
      "Math"
    }
  })

  # District data for the selected year, with median distance and rank attached.
  map_year_scores <- reactive({
    req(input$map_year, input$map_subject)

    score_data |>
      filter(year == input$map_year) |>
      mutate(
        selected_score = .data[[input$map_subject]],
        median_score = median(selected_score, na.rm = TRUE),
        distance_from_median = selected_score - median_score,
        rank = if_else(
          is.na(selected_score),
          NA_integer_,
          as.integer(rank(-selected_score, ties.method = "min", na.last = "keep"))
        ),
        n_ranked = sum(!is.na(selected_score))
      )
  })

  map_data <- reactive({
    district_shapes |>
      left_join(map_year_scores(), by = "nces_lea")
  })

  rate_palette <- colorNumeric(
    palette = rev(viridisLite::mako(256)),
    domain = score_domain,
    na.color = "#d7dde3"
  )

  median_palette <- colorNumeric(
    palette = colorRampPalette(c("#c4453c", "#f6f4ef", "#23788a"))(256),
    domain = c(-median_domain_max, median_domain_max),
    na.color = "#d7dde3"
  )

  map_labels <- function(data) {
    label <- subject_label()

    lapply(seq_len(nrow(data)), function(i) {
      score <- data$selected_score[i]
      distance <- data$distance_from_median[i]
      rank <- data$rank[i]
      n_ranked <- data$n_ranked[i]

      rank_text <- if (is.na(rank)) "" else paste0(
        "Rank: #", rank, " of ", n_ranked, " districts<br>"
      )

      HTML(paste0(
        "<strong>", htmlEscape(coalesce(data$name[i], data$NAME[i], "Unknown district")), "</strong><br>",
        label, " pass rate (", input$map_year, "): ",
        if (is.na(score)) "no data" else format_pct(score), "<br>",
        "Vs. state median: ", format_pts(distance), "<br>",
        rank_text,
        "<span style='color:#8b97a3'>Click for full profile &amp; trend</span>"
      ))
    })
  }

  draw_map_layers <- function(map, data) {
    label <- subject_label()

    if (identical(input$map_metric, "median")) {
      fill_values <- pmax(pmin(data$distance_from_median, median_domain_max), -median_domain_max)
      pal <- median_palette
      legend_values <- c(-median_domain_max, median_domain_max)
      legend_title <- paste0(label, " pass rate, pts from<br>state median (", input$map_year, ")")
      legend_format <- labelFormat(suffix = " pts")
    } else {
      fill_values <- data$selected_score
      pal <- rate_palette
      legend_values <- score_domain
      legend_title <- paste0(label, " pass<br>rate (", input$map_year, ")")
      legend_format <- labelFormat(suffix = "%")
    }

    map |>
      addPolygons(
        layerId = ~nces_lea,
        fillColor = pal(fill_values),
        fillOpacity = 0.84,
        color = "#ffffff",
        weight = 0.7,
        opacity = 0.95,
        label = map_labels(data),
        labelOptions = labelOptions(direction = "auto", sticky = TRUE, textsize = "13px"),
        highlightOptions = highlightOptions(
          weight = 2.2,
          color = "#1f3a52",
          fillOpacity = 0.95,
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = legend_values,
        title = legend_title,
        labFormat = legend_format,
        na.label = "No data",
        opacity = 0.9
      )
  }

  city_markers <- function(map) {
    map |>
      addLabelOnlyMarkers(
        data = reference_cities,
        lng = ~lng,
        lat = ~lat,
        group = "cities",
        label = ~city,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          className = "city-label"
        )
      )
  }

  output$explore_map <- renderLeaflet({
    data <- isolate(map_data())

    map <- leaflet(
      data,
      options = leafletOptions(zoomControl = TRUE, minZoom = 6)
    ) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = TRUE)) |>
      draw_map_layers(data) |>
      city_markers() |>
      setView(lng = -120.75, lat = 47.4, zoom = 7) |>
      setMaxBounds(-128, 43.5, -113.5, 50.5)

    map
  })

  observeEvent(list(input$map_year, input$map_subject, input$map_metric), {
    data <- map_data()

    leafletProxy("explore_map", data = data) |>
      clearShapes() |>
      clearControls() |>
      clearPopups() |>
      draw_map_layers(data)
  }, ignoreInit = TRUE)

  observeEvent(input$map_cities, {
    proxy <- leafletProxy("explore_map")
    if (isTRUE(input$map_cities)) {
      showGroup(proxy, "cities")
    } else {
      hideGroup(proxy, "cities")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$map_reset, {
    leafletProxy("explore_map") |>
      flyTo(lng = -120.75, lat = 47.4, zoom = 7)
  })

  output$map_statbar <- renderUI({
    data <- map_year_scores()
    label <- subject_label()

    ranked <- data |> filter(!is.na(selected_score))
    median_rate <- median(ranked$selected_score, na.rm = TRUE)

    top <- ranked |> slice_max(selected_score, n = 1, with_ties = FALSE)
    bottom <- ranked |> slice_min(selected_score, n = 1, with_ties = FALSE)

    stat <- function(label_text, value, detail = NULL) {
      div(
        class = "map-stat",
        div(class = "stat-label", label_text),
        div(class = "stat-value", value),
        if (!is.null(detail)) div(class = "stat-detail", detail)
      )
    }

    div(
      class = "map-statbar",
      stat(
        sprintf("Median district pass rate · %s %d", label, input$map_year),
        format_pct(median_rate)
      ),
      stat(
        "Highest district",
        if (nrow(top) == 0) "—" else format_pct(top$selected_score[1]),
        if (nrow(top) == 0) NULL else top$name[1]
      ),
      stat(
        "Lowest district",
        if (nrow(bottom) == 0) "—" else format_pct(bottom$selected_score[1]),
        if (nrow(bottom) == 0) NULL else bottom$name[1]
      ),
      stat(
        "Districts reporting",
        comma(nrow(ranked)),
        sprintf("of %s mapped", comma(nrow(district_shapes)))
      )
    )
  })

  district_popup_html <- function(lea) {
    row <- map_year_scores() |> filter(nces_lea == lea)
    shape_name <- district_shapes$NAME[district_shapes$nces_lea == lea][1]

    trend <- score_data |>
      filter(nces_lea == lea) |>
      arrange(year)

    display_name <- coalesce(
      if (nrow(row) > 0) row$name[1] else NA_character_,
      if (nrow(trend) > 0) trend$name[1] else NA_character_,
      shape_name,
      "Unknown district"
    )
    county <- coalesce(
      if (nrow(row) > 0) row$county[1] else NA_character_,
      if (nrow(trend) > 0) trend$county[1] else NA_character_
    )

    row1 <- if (nrow(row) > 0) row[1, ] else NULL

    stat_row <- function(label_text, value) {
      sprintf("<tr><td>%s</td><td class='popup-num'>%s</td></tr>", label_text, value)
    }

    year_rows <- if (!is.null(row1)) {
      math_rank <- {
        ranks <- map_year_scores() |>
          mutate(
            r = if_else(is.na(math_score), NA_integer_, as.integer(rank(-math_score, ties.method = "min", na.last = "keep"))),
            n = sum(!is.na(math_score))
          ) |>
          filter(nces_lea == lea)
        ranks
      }

      paste0(
        sprintf("<div class='popup-section'>Results, %d</div>", input$map_year),
        "<table>",
        stat_row("Math/ELA average pass rate", format_pct(row1$both_score)),
        stat_row(
          "Math pass rate",
          paste0(
            format_pct(row1$math_score),
            if (!is.na(math_rank$r[1])) sprintf(" <span style='font-weight:500;color:#8b97a3'>(#%d of %d)</span>", math_rank$r[1], math_rank$n[1]) else ""
          )
        ),
        stat_row("ELA pass rate", format_pct(row1$ela_score)),
        stat_row(paste0(subject_label(), " vs. state median"), format_pts(row1$distance_from_median)),
        stat_row("Students expected to test", if (is.na(row1$expected_to_test) || row1$expected_to_test == 0) "—" else comma(row1$expected_to_test)),
        "</table>",
        "<div class='popup-section'>District profile</div>",
        "<table>",
        stat_row("Enrollment", if (is.na(row1$enrollment)) "—" else comma(round(row1$enrollment))),
        stat_row("Low-income students (FRPL)", format_pct(row1$pct_low_income, acc = 1)),
        stat_row("Avg. teacher salary (2015 $)", format_dollars(row1$salary_cpi)),
        stat_row("Instructional $ per student (2015 $)", format_dollars(row1$spend_per_student)),
        stat_row("Avg. teacher experience", if (is.na(row1$teacher_experience)) "—" else paste0(number(row1$teacher_experience, accuracy = 0.1), " yrs")),
        "</table>"
      )
    } else {
      "<div class='popup-section'>No data reported for this year</div>"
    }

    spark <- if (nrow(trend) >= 2) {
      paste0(
        "<div class='popup-section'>Ten-year trend</div>",
        "<div class='popup-spark'>",
        sparkline_svg(trend$year, trend$math_score, trend$ela_score),
        "<div class='spark-caption'>",
        "<span style='color:#2a7f8f;font-weight:700'>— Math</span> &nbsp; ",
        "<span style='color:#2a9d8f;font-weight:700'>- - ELA</span>",
        " &nbsp;·&nbsp; gap = Covid testing pause</div>",
        "</div>"
      )
    } else {
      ""
    }

    paste0(
      "<div class='district-popup'>",
      "<h4>", htmlEscape(display_name), "</h4>",
      "<p class='popup-county'>",
      if (!is.na(county)) paste0(htmlEscape(county), " County") else "Washington",
      "</p>",
      year_rows,
      spark,
      "</div>"
    )
  }

  show_district_popup <- function(lea, lng = NULL, lat = NULL) {
    if (is.null(lng) || is.null(lat)) {
      bb <- district_bounds[district_bounds$nces_lea == lea, ]
      if (nrow(bb) == 0) return(invisible(NULL))
      lng <- (bb$xmin + bb$xmax) / 2
      lat <- (bb$ymin + bb$ymax) / 2
    }

    leafletProxy("explore_map") |>
      clearPopups() |>
      addPopups(
        lng = lng,
        lat = lat,
        popup = district_popup_html(lea),
        options = popupOptions(maxWidth = 320, closeButton = TRUE)
      )
  }

  observeEvent(input$explore_map_shape_click, {
    click <- input$explore_map_shape_click
    req(click$id)
    show_district_popup(click$id, click$lng, click$lat)
  })

  observeEvent(input$map_search, {
    req(nzchar(input$map_search))
    lea <- input$map_search
    bb <- district_bounds[district_bounds$nces_lea == lea, ]
    req(nrow(bb) > 0)

    leafletProxy("explore_map") |>
      flyToBounds(bb$xmin, bb$ymin, bb$xmax, bb$ymax, options = list(padding = c(60, 60)))

    show_district_popup(lea)
  }, ignoreInit = TRUE)
}

app <- shinyApp(ui, server)

if (interactive()) {
  runApp(app)
}
