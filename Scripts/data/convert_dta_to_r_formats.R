in_dir <- file.path("Data", "raw")
out_dir <- file.path("Data", "converted")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

fred_cpi_url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPIAUCSL"

money_columns <- c(
  "meantotalsalary",
  "meancertbasesalary",
  "totalrev",
  "tfedrev",
  "tstaterev",
  "c01",
  "c05",
  "c07",
  "c08",
  "c10",
  "tlocalrev",
  "t06",
  "totalexp",
  "tinstruction",
  "tsupport",
  "tcapex",
  "tsalaries",
  "tsalariesinstruction"
)

load_annual_cpi <- function() {
  cpi_monthly <- readr::read_csv(fred_cpi_url, show_col_types = FALSE)

  required_columns <- c("observation_date", "CPIAUCSL")
  if (!all(required_columns %in% names(cpi_monthly))) {
    stop("FRED CPI download did not include the expected columns.", call. = FALSE)
  }

  cpi_monthly |>
    dplyr::mutate(
      observation_date = as.Date(observation_date),
      year = as.integer(format(observation_date, "%Y")),
      CPIAUCSL = as.numeric(CPIAUCSL)
    ) |>
    dplyr::filter(!is.na(year), !is.na(CPIAUCSL)) |>
    dplyr::group_by(year) |>
    dplyr::summarize(cpi = mean(CPIAUCSL, na.rm = TRUE), .groups = "drop")
}

add_cpi_adjusted_money_columns <- function(d, annual_cpi) {
  if (!"year" %in% names(d)) {
    return(d)
  }

  available_money_columns <- intersect(money_columns, names(d))
  if (length(available_money_columns) == 0) {
    return(d)
  }

  years_with_money <- d |>
    dplyr::filter(dplyr::if_any(
      dplyr::all_of(available_money_columns),
      ~ !is.na(.x)
    )) |>
    dplyr::pull(year)

  base_year <- min(years_with_money, na.rm = TRUE)
  if (!is.finite(base_year)) {
    return(d)
  }

  base_cpi <- annual_cpi$cpi[annual_cpi$year == base_year]
  if (length(base_cpi) == 0 || is.na(base_cpi)) {
    stop("FRED CPI data is missing the base year: ", base_year, call. = FALSE)
  }

  d |>
    dplyr::left_join(annual_cpi, by = "year") |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(available_money_columns),
        ~ .x * base_cpi / cpi,
        .names = "{.col}_cpi_adjusted"
      )
    ) |>
    dplyr::select(-cpi)
}

files <- list.files(in_dir, pattern = "\\.dta$", full.names = TRUE)

if (length(files) == 0) {
  stop("No .dta files found in ", normalizePath(in_dir, mustWork = FALSE))
}

message("Downloading CPIAUCSL from FRED...")
annual_cpi <- load_annual_cpi()

for (f in files) {
  message("Reading: ", f)

  d <- haven::read_dta(f) |>
    haven::zap_labels() |>
    add_cpi_adjusted_money_columns(annual_cpi)

  base <- tools::file_path_sans_ext(basename(f))

  csv_path <- file.path(out_dir, paste0(base, ".csv"))
  rds_path <- file.path(out_dir, paste0(base, ".rds"))

  readr::write_csv(d, csv_path, na = "")
  saveRDS(d, rds_path)

  message("Wrote: ", csv_path)
  message("Wrote: ", rds_path)
}
