# Entry point for shinyapps.io deployment. The deployed bundle mirrors the
# project layout (Data/, Outputs/, Apps/), so the dashboard's project-root
# detection works unchanged.
source(file.path("Apps", "shiny", "shiny_results_dashboard.R"), local = TRUE)
app
