# This file is a package installer for required CRAN packages used by this Shiny app.

required_packages <- c(
  "shiny", "shinydashboard", "shinyWidgets", "tidyverse",
  "plotly", "viridis", "rvest", "countrycode", "WDI"
)

installed <- rownames(installed.packages())
missing <- setdiff(required_packages, installed)

if (length(missing) == 0) {
  message("All required packages are already installed.")
} else {
  message("Installing missing packages:", paste(missing, collapse = ", "))
  install.packages(missing, dependencies = TRUE)
  message("Done. You may need to restart your R session.")
}
