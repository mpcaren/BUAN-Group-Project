in_dir <- "Data"
out_dir <- file.path(in_dir, "converted")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

files <- list.files(in_dir, pattern = "\\.dta$", full.names = TRUE)

if (length(files) == 0) {
  stop("No .dta files found in ", normalizePath(in_dir, mustWork = FALSE))
}

for (f in files) {
  message("Reading: ", f)

  d <- haven::read_dta(f)
  base <- tools::file_path_sans_ext(basename(f))

  csv_path <- file.path(out_dir, paste0(base, ".csv"))
  rds_path <- file.path(out_dir, paste0(base, ".rds"))

  readr::write_csv(haven::zap_labels(d), csv_path, na = "")
  saveRDS(d, rds_path)

  message("Wrote: ", csv_path)
  message("Wrote: ", rds_path)
}
