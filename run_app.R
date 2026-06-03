# Starts the Shiny dashboard on a fixed local address.
# Run this file when launching the app from Rscript or PowerShell.

shiny::runApp(
  "shiny_dashboard_2.R",
  host = "127.0.0.1",
  port = 3838,
  launch.browser = FALSE
)
