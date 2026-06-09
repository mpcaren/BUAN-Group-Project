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

score_data <- readRDS(data_path) |>
  mutate(
    nces_lea = as.character(nces_lea),
    math_score = metMATH_pct * 100,
    ela_score = metELA_pct * 100
  ) |>
  group_by(year, nces_lea, name, county) |>
  summarize(
    math_score = mean(math_score, na.rm = TRUE),
    ela_score = mean(ela_score, na.rm = TRUE),
    expected_to_test = sum(expectedtotest_n, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    math_score = if_else(is.nan(math_score), NA_real_, math_score),
    ela_score = if_else(is.nan(ela_score), NA_real_, ela_score)
  )

district_shapes <- st_read(shape_path, quiet = TRUE) |>
  st_transform(4326) |>
  mutate(nces_lea = GEOID)

available_years <- sort(unique(score_data$year))

addResourcePath("figures", figures_dir)

figure_path <- function(filename) {
  paste0("figures/", URLencode(filename, reserved = TRUE))
}

result_figure <- function(filename, alt) {
  tags$img(
    class = "result-figure",
    src = figure_path(filename),
    alt = alt
  )
}

step_section <- function(number, title, description, ...) {
  div(
    class = "step-section",
    div(
      class = "step-heading",
      span(class = "step-number", number),
      div(
        h2(title),
        p(description)
      )
    ),
    ...
  )
}

chart_card <- function(title, filename, alt, class = "") {
  div(
    class = paste("chart-card", class),
    h3(title),
    result_figure(filename, alt)
  )
}

map_controls <- function(prefix) {
  div(
    class = "map-controls",
    div(
      class = "subject-control",
      radioButtons(
        inputId = paste0(prefix, "_subject"),
        label = "Subject",
        choices = c("Math" = "math_score", "ELA" = "ela_score"),
        selected = "math_score",
        inline = TRUE
      )
    ),
    div(
      class = "year-control",
      sliderInput(
        inputId = paste0(prefix, "_year"),
        label = "Year",
        min = min(available_years),
        max = max(available_years),
        value = max(available_years),
        step = 1,
        sep = "",
        ticks = TRUE,
        animate = animationOptions(interval = 1200, loop = TRUE)
      )
    )
  )
}

dashboard_css <- "
  body {
    color: #2f3437;
    background: #f4f7f8;
  }

  .navbar {
    margin-bottom: 0;
    border: 0;
    border-radius: 0;
    background: #243b53;
  }

  .navbar-default .navbar-brand,
  .navbar-default .navbar-nav > li > a {
    color: #f8fafc;
  }

  .navbar-default .navbar-nav > .active > a,
  .navbar-default .navbar-nav > .active > a:hover,
  .navbar-default .navbar-nav > li > a:hover {
    color: #ffffff;
    background: #2a9d8f;
  }

  .dashboard-shell {
    max-width: 1500px;
    margin: 0 auto;
    padding: 28px 28px 48px;
  }

  .dashboard-intro {
    padding: 10px 0 28px;
    border-bottom: 1px solid #d9dee5;
  }

  .dashboard-intro h1 {
    margin: 0 0 8px;
    font-size: 32px;
    font-weight: 700;
    color: #243b53;
  }

  .dashboard-intro p {
    max-width: 920px;
    margin: 0;
    color: #697077;
    font-size: 16px;
    line-height: 1.55;
  }

  .step-section {
    padding: 30px 0 10px;
  }

  .step-heading {
    display: flex;
    gap: 14px;
    align-items: flex-start;
    margin-bottom: 18px;
  }

  .step-number {
    display: inline-flex;
    width: 34px;
    height: 34px;
    align-items: center;
    justify-content: center;
    flex: 0 0 34px;
    border-radius: 50%;
    background: #2a9d8f;
    color: white;
    font-weight: 700;
  }

  .step-heading h2 {
    margin: 0 0 4px;
    font-size: 23px;
    color: #243b53;
  }

  .step-heading p {
    margin: 0;
    color: #697077;
  }

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
    padding: 14px;
    border: 1px solid #d9dee5;
    border-radius: 6px;
    background: #ffffff;
  }

  .chart-card h3 {
    margin: 0 0 10px;
    font-size: 16px;
    font-weight: 650;
    color: #243b53;
  }

  .result-figure {
    display: block;
    width: 100%;
    height: auto;
    max-height: 680px;
    object-fit: contain;
  }

  .chart-browser {
    max-width: 1450px;
    margin: 0 auto;
    padding: 24px 28px 42px;
  }

  .chart-browser .form-group {
    max-width: 460px;
  }

  .chart-browser-frame {
    padding: 16px;
    border: 1px solid #d9dee5;
    border-radius: 6px;
    background: #ffffff;
  }

  .map-page {
    padding: 20px 24px 30px;
  }

  .map-header {
    max-width: 1100px;
    margin-bottom: 14px;
  }

  .map-header h2 {
    margin: 0 0 5px;
    color: #243b53;
    font-size: 25px;
  }

  .map-header p {
    margin: 0;
    color: #697077;
  }

  .map-controls {
    display: flex;
    gap: 24px;
    align-items: end;
    padding: 12px 16px;
    border: 1px solid #d9dee5;
    border-bottom: 0;
    border-radius: 6px 6px 0 0;
    background: #ffffff;
  }

  .map-controls .form-group,
  .map-controls .shiny-input-container {
    margin-bottom: 0;
  }

  .subject-control {
    flex: 0 0 210px;
  }

  .year-control {
    flex: 1 1 auto;
  }

  .year-control .shiny-input-container {
    width: 100%;
    max-width: 760px;
  }

  .map-frame {
    overflow: hidden;
    border: 1px solid #d9dee5;
    border-radius: 0 0 6px 6px;
    background: #ffffff;
  }

  @media (max-width: 900px) {
    .dashboard-shell,
    .chart-browser,
    .map-page {
      padding-left: 14px;
      padding-right: 14px;
    }

    .chart-grid {
      grid-template-columns: minmax(0, 1fr);
    }

    .map-controls {
      display: block;
    }

    .subject-control {
      width: 100%;
    }
  }
"

dashboard_js <- "
  $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function() {
    setTimeout(function() {
      ['score_map', 'median_map'].forEach(function(id) {
        var widget = HTMLWidgets.find('#' + id);
        if (widget && widget.getMap) {
          widget.getMap().invalidateSize();
        }
      });
      $(window).trigger('resize');
    }, 300);
  });
"

chart_choices <- c(
  "Test scores over time" = "wa_test_scores_over_time.png",
  "Salary and student outcomes" = "wa_salary_vs_scores_scatter.png",
  "Spending and student outcomes" = "viz_spend_final.png",
  "Spending by poverty quartile" = "viz_spend_by_poverty.png",
  "Student demographics by district type" = "viz4_demo_comparison.png",
  "Pay gap and learning gap" = "viz4_wealth_poverty_profiles.png"
)

ui <- navbarPage(
  title = "Washington Education Results",
  id = "main_navigation",
  collapsible = TRUE,
  header = tags$head(
    tags$style(HTML(dashboard_css)),
    tags$script(HTML(dashboard_js))
  ),
  tabPanel(
    "Overview",
    div(
      class = "dashboard-shell",
      div(
        class = "dashboard-intro",
        h1("Washington district outcomes, resources, and equity"),
        p("Use the navigation above to move through outcomes, resources, equity, and interactive maps. Dollar values shown in the results are CPI-adjusted to 2015.")
      ),
      step_section(
        "1",
        "Outcomes",
        "See how Math and ELA performance changed across the study period."
      ),
      step_section(
        "2",
        "Resources",
        "Examine whether salary and instructional spending are associated with stronger student outcomes."
      ),
      step_section(
        "3",
        "Equity",
        "Compare resources, demographics, and outcomes across district poverty levels."
      ),
      step_section(
        "4",
        "Explore the geography",
        "Open the nested Explore Maps menu to inspect district requirement rates and distance from the district median."
      )
    )
  ),
  tabPanel(
    "Outcomes",
    div(
      class = "dashboard-shell",
      div(
        class = "dashboard-intro",
        h1("Student outcomes"),
        p("Statewide Math and ELA requirement rates across the study period.")
      ),
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
    div(
      class = "dashboard-shell",
      div(
        class = "dashboard-intro",
        h1("Resources and student outcomes"),
        p("Teacher salary and instructional spending compared with composite student outcomes. Dollar values are CPI-adjusted to 2015.")
      ),
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
    div(
      class = "dashboard-shell",
      div(
        class = "dashboard-intro",
        h1("Distribution and equity"),
        p("Resources, demographics, and outcomes compared across district poverty levels.")
      ),
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
    "All Charts",
    div(
      class = "chart-browser",
      h2("Browse full-size results"),
      selectInput("selected_chart", "Result", choices = chart_choices),
      div(class = "chart-browser-frame", uiOutput("selected_chart_output"))
    )
  ),
  navbarMenu(
    "Explore Maps",
    tabPanel(
      "Requirement Rates",
      div(
        class = "map-page",
        div(
          class = "map-header",
          h2("Students meeting state exam requirements"),
          p("Choose a subject and year, then hover over a district to inspect its requirement rate.")
        ),
        map_controls("score"),
        div(class = "map-frame", leafletOutput("score_map", height = "720px"))
      )
    ),
    tabPanel(
      "Median Comparison",
      div(
        class = "map-page",
        div(
          class = "map-header",
          h2("District requirement rates compared with the median"),
          p("Districts are colored by percentage-point distance from the median district rate for the selected subject and year.")
        ),
        map_controls("median"),
        div(class = "map-frame", leafletOutput("median_map", height = "720px"))
      )
    )
  )
)

server <- function(input, output, session) {
  output$selected_chart_output <- renderUI({
    req(input$selected_chart)
    result_figure(input$selected_chart, names(chart_choices)[chart_choices == input$selected_chart])
  })

  score_column <- function(subject) {
    if (identical(subject, "ela_score")) "ela_score" else "math_score"
  }

  subject_label <- function(subject) {
    if (identical(score_column(subject), "ela_score")) "ELA" else "Math"
  }

  score_map_data <- reactive({
    district_shapes |>
      left_join(
        score_data |> filter(year == input$score_year),
        by = "nces_lea"
      )
  })

  score_labels <- reactive({
    data <- score_map_data()
    selected_column <- score_column(input$score_subject)
    selected_label <- subject_label(input$score_subject)

    lapply(seq_len(nrow(data)), function(i) {
      score <- data[[selected_column]][i]
      score_text <- if (is.na(score)) {
        "No requirement rate available"
      } else {
        paste0(number(score, accuracy = 0.1), "%")
      }

      HTML(paste0(
        "<strong>", htmlEscape(coalesce(data$name[i], data$NAME[i], "Unknown district")), "</strong><br>",
        selected_label, " requirement rate: ", score_text, "<br>",
        "Year: ", input$score_year
      ))
    })
  })

  score_palette <- function() {
    colorNumeric(
      palette = rev(viridisLite::viridis(256)),
      domain = c(5, 85),
      na.color = "#d7dde3"
    )
  }

  draw_score_map <- function(map, data) {
    selected_column <- score_column(input$score_subject)
    selected_label <- subject_label(input$score_subject)
    values <- data[[selected_column]]
    pal <- score_palette()

    map |>
      addPolygons(
        layerId = ~nces_lea,
        fillColor = pal(values),
        fillOpacity = 0.82,
        color = "#ffffff",
        weight = 0.7,
        opacity = 0.95,
        label = score_labels(),
        labelOptions = labelOptions(direction = "auto", sticky = TRUE, textsize = "13px"),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#243b53",
          fillOpacity = 0.92,
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = c(5, 85),
        title = paste(selected_label, "requirement rate"),
        labFormat = labelFormat(suffix = "%")
      )
  }

  output$score_map <- renderLeaflet({
    data <- score_map_data()

    leaflet(data, options = leafletOptions(zoomControl = TRUE)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      draw_score_map(data) |>
      setView(lng = -120.75, lat = 47.4, zoom = 7)
  })

  observeEvent(list(input$score_year, input$score_subject), {
    data <- score_map_data()

    leafletProxy("score_map", data = data) |>
      clearShapes() |>
      clearControls() |>
      draw_score_map(data)
  }, ignoreInit = TRUE)

  median_year_scores <- reactive({
    selected_column <- score_column(input$median_subject)

    score_data |>
      filter(year == input$median_year) |>
      mutate(
        selected_score = .data[[selected_column]],
        median_score = median(selected_score, na.rm = TRUE),
        distance_from_median = selected_score - median_score
      )
  })

  median_map_data <- reactive({
    district_shapes |>
      left_join(median_year_scores(), by = "nces_lea")
  })

  median_labels <- reactive({
    data <- median_map_data()
    selected_label <- subject_label(input$median_subject)

    lapply(seq_len(nrow(data)), function(i) {
      score <- data$selected_score[i]
      median_score <- data$median_score[i]
      distance <- data$distance_from_median[i]

      score_text <- if (is.na(score)) "No requirement rate available" else paste0(number(score, accuracy = 0.1), "%")
      median_text <- if (is.na(median_score)) "No median available" else paste0(number(median_score, accuracy = 0.1), "%")
      distance_text <- if (is.na(distance)) "No distance available" else paste0(number(distance, accuracy = 0.1, style_positive = "plus"), " pts")

      HTML(paste0(
        "<strong>", htmlEscape(coalesce(data$name[i], data$NAME[i], "Unknown district")), "</strong><br>",
        selected_label, " requirement rate: ", score_text, "<br>",
        "Median district rate: ", median_text, "<br>",
        "Distance from median: ", distance_text, "<br>",
        "Year: ", input$median_year
      ))
    })
  })

  median_palette <- function() {
    colorNumeric(
      palette = colorRampPalette(c("#d65f5f", "#f7f7f7", "#3b82a0"))(256),
      domain = c(-50, 50),
      na.color = "#d7dde3"
    )
  }

  draw_median_map <- function(map, data) {
    selected_label <- subject_label(input$median_subject)
    pal <- median_palette()

    map |>
      addPolygons(
        layerId = ~nces_lea,
        fillColor = pal(data$distance_from_median),
        fillOpacity = 0.82,
        color = "#ffffff",
        weight = 0.7,
        opacity = 0.95,
        label = median_labels(),
        labelOptions = labelOptions(direction = "auto", sticky = TRUE, textsize = "13px"),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#243b53",
          fillOpacity = 0.92,
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = c(-50, 50),
        title = paste(selected_label, "pts from median"),
        labFormat = labelFormat(suffix = " pts")
      )
  }

  output$median_map <- renderLeaflet({
    data <- median_map_data()

    leaflet(data, options = leafletOptions(zoomControl = TRUE)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      draw_median_map(data) |>
      setView(lng = -120.75, lat = 47.4, zoom = 7)
  })

  observeEvent(list(input$median_year, input$median_subject), {
    data <- median_map_data()

    leafletProxy("median_map", data = data) |>
      clearShapes() |>
      clearControls() |>
      draw_median_map(data)
  }, ignoreInit = TRUE)
}

app <- shinyApp(ui, server)

if (interactive()) {
  runApp(app)
}
