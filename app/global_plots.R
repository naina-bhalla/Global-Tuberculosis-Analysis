# This file contains most of the plots on the the different tabs.

library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(viridis)

# ---------- Trend Plot ----------
trendPlot <- function(data, country, metric, metric_names) {
  df <- data %>% filter(country_who %in% country) %>% filter(!is.na(.data[[metric]]))
  
  p <- ggplot(df, aes(x = year, y = .data[[metric]], color = country_who)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    labs(title = paste(metric_names[metric], "Trend"),
         x = "Year", y = metric_names[metric], color = "Country") +
    theme_minimal(base_size = 14)
  
  ggplotly(p, tooltip = c("x", "y", "color"))
}

# ---------- Socioeconomic Correlation ----------
socioPlot <- function(data, country, x_var, y_vars, metric_names, wdi_labels) {
  df <- data %>%
    filter(country_who %in% country) %>%
    select(country_who, x = .data[[x_var]], all_of(y_vars)) %>%
    pivot_longer(cols = all_of(y_vars), names_to = "metric", values_to = "value") %>%
    filter(!is.na(x), !is.na(value)) %>%
    mutate(metric = factor(metric, levels = y_vars, labels = metric_names[y_vars]))
  
  p <- ggplot(df, aes(x = x, y = value, color = country_who, linetype = metric)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = paste("Metrics vs", wdi_labels[x_var]),
      x = wdi_labels[x_var],
      y = "Value",
      color = "Country",
      linetype = "Metric"
    ) +
    theme_minimal(base_size = 14)
  
  ggplotly(p)
}

# ---------- Drug Resistance ----------
rrPlot <- function(data, country, metric, rr_labels) {
  df <- data %>% filter(country_who %in% country) %>% filter(!is.na(.data[[metric]]))
  
  p <- ggplot(df, aes(x = year, y = .data[[metric]], color = country_who)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    labs(title = paste(rr_labels[metric], "Trend"),
         x = "Year", y = rr_labels[metric], color = "Country") +
    theme_minimal(base_size = 14)
  
  ggplotly(p, tooltip = c("x", "y", "color"))
}

# ---------- COVID Impact ----------
covidPlot <- function(data, country) {
  df <- data %>%
    filter(country_who %in% country) %>%
    filter(!is.na(e_inc_100k) & !is.na(c_newinc_100k)) %>%
    mutate(underreporting = e_inc_100k - c_newinc_100k)
  
  covid_df <- df %>%
    select(year, country_who, e_inc_100k, c_newinc_100k, underreporting) %>%
    pivot_longer(cols = c(e_inc_100k, c_newinc_100k, underreporting),
                 names_to = "metric", values_to = "value") %>%
    mutate(metric = factor(metric,
                           levels = c("e_inc_100k", "c_newinc_100k", "underreporting"),
                           labels = c("Estimated Incidence", "Notifications", "Estimated Underreporting")))
  
  linetypes <- c("Estimated Incidence" = "solid",
                 "Notifications" = "solid",
                 "Estimated Underreporting" = "dash")
  
  n_countries <- length(country)
  colors <- viridis(n_countries)
  names(colors) <- country
  
  p <- ggplot(covid_df, aes(x = year, y = value, color = country_who, linetype = metric)) +
    geom_line(linewidth = 1.2) +
    scale_linetype_manual(values = linetypes) +
    scale_color_manual(values = colors) +
    labs(title = "COVID-19 Impact on TB Notifications",
         x = "Year", y = "Cases per 100k",
         color = "Country", linetype = "Metric") +
    theme_minimal(base_size = 14)
  
  ggplotly(p, tooltip = c("x", "y", "color", "linetype"))
}
