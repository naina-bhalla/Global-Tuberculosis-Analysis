# Global Tuberculosis Analysis Dashboard
## Group 24: MTH208 Course Project

## Overview

This app analyzes global and regional trends in Tuberculosis (TB) using WHO and World Bank data. It provides:

- An interactive Shiny dashboard for data exploration and visualization.
- Reproducible scripts for data acquisition, cleaning, and EDA.
- Exportable figures and tables suitable for a written report.

## Quickstart: run the app locally

1. Install the required R packages (run once):

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyWidgets", "tidyverse",
  "plotly", "viridis", "rvest", "countrycode", "WDI"
))
```

Recommended (one-line): run the included installer which checks for missing packages and installs them. In RStudio you can open the project or use Session → Set Working Directory → To Project Directory.:

```r
source("code/install_packages.R")
```

2. From the project root, start the app:

```r
shiny::runApp("app")
```

Tip: open `app/app.R` in RStudio and click "Run App".

## Repository structure (important files)

app/
- `app.R`: App launcher
- `ui.R`: Shiny UI 
- `server.R`: Shiny server logic
- `global.R`: Shared data loading and labels
- `global_plots.R`, `home_plots.R`: Plotting functions
- `scraping.R`: data extraction and combination
- `data/`: downloaded CSVs (including `who_tb_global.csv`)

code/
- `install_packages.R`: To download packages
- `scraping.R`: To run the data extraction separately

## Data sources

- WHO TB datasets: CSV snapshots included in `app/data/who_data/`.
- World Bank (WDI) indicators: retrieved via the `WDI` package in `scraping.R`.

All original source filenames and retrieval codes are in `scraping.R`.

## Methodology (implemented)

1. Data acquisition and merging (`scraping.R`)
   - Reads WHO CSV snapshots from `app/data/who_data/` and selects TB and budget indicators.
   - Downloads World Bank indicators using `WDI` and merges them with WHO data.
   - Adds ISO codes (`countrycode`) and writes the combined dataset to `app/data/who_tb_global.csv`.

2. Cleaning & harmonization (`scraping.R`, `global.R`)
   - Harmonizes country names via `countrycode` where possible.
   - Missing values are handled during summarization/plotting (e.g., `na.rm = TRUE`).
   - Per-100k indicators are exposed as columns (e.g., `e_inc_100k`).

3. Exploratory analysis and derived metrics (`global_plots.R`, `home_plots.R`)
   - Trends, region filters, and drug-resistance trends are implemented in plotting functions.
   - Global summaries (means, top-N country lists) and underreporting (e_inc_100k - c_newinc_100k) are computed for visualizations.

4. Visualization & UI
   - Plots are constructed with `ggplot2` and converted to interactive plots via `plotly::ggplotly()`.
   - The Shiny app (`ui.R` + `server.R`) wires inputs to these plotting functions and displays value boxes and summary panels.

For full details, inspect `scraping.R`, `global.R`, `global_plots.R`, and `home_plots.R`.

## Reproducibility

- R version used for development: R 4.3.2 (scripts should work on R >= 4.0).
- To reproduce figures and the app output:

  1. (Optional) Update and run `scraping.R` to refresh the merged dataset.
  2. Ensure `app/data/who_tb_global.csv` exists (or re-generate with `scraping.R`).
  3. Run `shiny::runApp("app")`.

## Limitations & ethics

- Only aggregated, public datasets are used, no personal or sensitive data.
- All data sources are open access, published by the WHO, World Bank, and UN agencies, and thus carry no privacy concerns. 



