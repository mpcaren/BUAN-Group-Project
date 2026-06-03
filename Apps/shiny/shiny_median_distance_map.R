run_median_distance_dashboard <- function() {
  required_packages <- c(
    "shiny",
    "leaflet",
    "sf",
    "dplyr",
    "htmltools",
    "scales"
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

  data_path <- file.path(
    "Data",
    "converted",
    "1. Data for Student Performance Paper 2015 2025.rds"
  )

  boundary_dir <- file.path("Data", "geo", "wa_unified_school_districts_2025")
  boundary_zip <- file.path(boundary_dir, "tl_2025_53_unsd.zip")
  boundary_url <- "https://www2.census.gov/geo/tiger/TIGER2025/UNSD/tl_2025_53_unsd.zip"

  if (!file.exists(data_path)) {
    stop("Could not find the merged RDS dataset at: ", data_path)
  }

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

  ui <- fluidPage(
    tags$head(
      tags$style(HTML("
        html, body, .container-fluid {
          height: 100%;
          margin: 0;
          padding: 0;
        }

        .container-fluid {
          display: flex;
          flex-direction: column;
        }

        .map-title {
          padding: 14px 18px 8px;
          border-bottom: 1px solid #dde3ea;
        }

        .map-title h1 {
          margin: 0;
          font-size: 22px;
          font-weight: 650;
        }

        .map-title p {
          margin: 4px 0 0;
          color: #52606d;
        }

        #district_map {
          flex: 1 1 auto;
          min-height: 0;
        }

        .controls {
          display: flex;
          gap: 24px;
          align-items: end;
          padding: 10px 22px 16px;
          border-top: 1px solid #dde3ea;
          background: #ffffff;
        }

        .controls .form-group,
        .controls .shiny-input-container {
          margin-bottom: 0;
        }

        .subject-control {
          min-width: 180px;
        }

        .slider-control {
          flex: 1 1 auto;
        }
      "))
    ),
    div(
      class = "map-title",
      h1("Washington District Scores Compared With Median"),
      p("Districts are colored by percentage-point distance from the median district score for the selected subject and year.")
    ),
    leafletOutput("district_map", height = "100%"),
    div(
      class = "controls",
      div(
        class = "subject-control",
        radioButtons(
          inputId = "subject",
          label = "Subject",
          choices = c("Math" = "math_score", "ELA" = "ela_score"),
          selected = "math_score",
          inline = TRUE
        )
      ),
      div(
        class = "slider-control",
        sliderInput(
          inputId = "year",
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
  )

  server <- function(input, output, session) {
    selected_score_column <- reactive({
      if (identical(input$subject, "ela_score")) "ela_score" else "math_score"
    })

    selected_subject_label <- reactive({
      if (identical(selected_score_column(), "ela_score")) "ELA" else "Math"
    })

    year_scores <- reactive({
      score_column <- selected_score_column()

      score_data |>
        filter(year == input$year) |>
        mutate(
          selected_score = .data[[score_column]],
          median_score = median(selected_score, na.rm = TRUE),
          distance_from_median = selected_score - median_score
        )
    })

    map_data <- reactive({
      district_shapes |>
        left_join(year_scores(), by = "nces_lea")
    })

    distance_palette <- function() {
      colorNumeric(
        palette = colorRampPalette(c("#b2182b", "#f7f7f7", "#2166ac"))(256),
        domain = c(-50, 50),
        na.color = "#d7dde3"
      )
    }

    district_labels <- function(data) {
      lapply(seq_len(nrow(data)), function(i) {
        subject_label <- selected_subject_label()
        score <- data$selected_score[i]
        median_score <- data$median_score[i]
        distance <- data$distance_from_median[i]

        score_text <- if (is.na(score)) "No score available" else paste0(number(score, accuracy = 0.1), "%")
        median_text <- if (is.na(median_score)) "No median available" else paste0(number(median_score, accuracy = 0.1), "%")
        distance_text <- if (is.na(distance)) {
          "No distance available"
        } else {
          paste0(number(distance, accuracy = 0.1, style_positive = "plus"), " pts")
        }

        HTML(paste0(
          "<strong>", htmlEscape(coalesce(data$name[i], data$NAME[i], "Unknown district")), "</strong><br>",
          "Mean ", subject_label, " score: ", score_text, "<br>",
          "Median district score: ", median_text, "<br>",
          "Distance from median: ", distance_text, "<br>",
          "Year: ", input$year
        ))
      })
    }

    draw_districts <- function(map, data, pal) {
      map |>
        addPolygons(
          layerId = ~nces_lea,
          fillColor = pal(data$distance_from_median),
          fillOpacity = 0.82,
          color = "#ffffff",
          weight = 0.7,
          opacity = 0.95,
          label = district_labels(data),
          labelOptions = labelOptions(
            direction = "auto",
            sticky = TRUE,
            textsize = "13px",
            opacity = 0.95
          ),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#1f2937",
            fillOpacity = 0.92,
            bringToFront = TRUE
          )
        ) |>
        addLegend(
          position = "bottomright",
          pal = pal,
          values = c(-50, 50),
          title = paste(selected_subject_label(), "pts from median"),
          labFormat = labelFormat(suffix = " pts")
        )
    }

    output$district_map <- renderLeaflet({
      data <- map_data()
      pal <- distance_palette()

      leaflet(data, options = leafletOptions(zoomControl = TRUE)) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        draw_districts(data, pal) |>
        setView(lng = -120.75, lat = 47.4, zoom = 7)
    })

    observeEvent(list(input$year, input$subject), {
      data <- map_data()
      pal <- distance_palette()

      leafletProxy("district_map", data = data) |>
        clearShapes() |>
        clearControls() |>
        draw_districts(data, pal)
    }, ignoreInit = TRUE)
  }

  shinyApp(ui, server)
}

if (interactive()) {
  run_median_distance_dashboard()
}
