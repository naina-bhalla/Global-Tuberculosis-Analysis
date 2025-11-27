# This contains all the plots shown on the homepage. These plots represent the global scenarios. 
# They aptly represent the conditions and severity of Tuberculosis in the world and work as a summary page.

library(ggplot2)
library(plotly)
library(dplyr)
library(viridis)

# ---------- Global Trend Plot ----------

globalTrendPlot <- function(data) {
  df <- data %>% 
    group_by(year) %>% 
    summarize(
      inc = mean(e_inc_100k, na.rm=TRUE), 
      mort = mean(e_mort_100k, na.rm=TRUE)
    )
  
  p <- ggplot(df, aes(x=year)) + 
    geom_line(aes(y=inc, color="Incidence"), linewidth=1.2) + 
    geom_line(aes(y=mort, color="Mortality"), linewidth=1.2) + 
    scale_color_manual(values=c("Incidence"="blue","Mortality"="red")) + 
    labs(x="Year", y="Cases per 100k", color="Metric") + 
    theme_minimal()
  
  ggplotly(p)
}

# ---------- TB vs GDP Plot ----------
tbGDPPlot <- function(data) {
  df <- data %>% filter(!is.na(NY.GDP.PCAP.CD) & !is.na(e_inc_100k))
  
  p <- ggplot(df, aes(x=NY.GDP.PCAP.CD, y=e_inc_100k)) + 
    geom_point(color="darkgreen", alpha=0.6) + 
    geom_smooth(method="lm", se=FALSE, color="black") + 
    labs(x="GDP per Capita", y="TB Incidence") + 
    theme_minimal()
  
  ggplotly(p)
}

# ---------- MDR-TB Trend ----------
mdrTrendPlot <- function(data) {
  df <- data %>% 
    group_by(year) %>% 
    filter(year >= 2015) %>%
    summarize(mdr = mean(e_rr_pct_new, na.rm=TRUE))
  
  p <- ggplot(df, aes(x=year, y=mdr)) + 
    geom_line(color="orange", linewidth=1.2) + 
    geom_point(color="orange") + 
    labs(x="Year", y="MDR-TB % (New Cases)") + 
    theme_minimal()
  
  ggplotly(p)
}

# ---------- COVID Underreporting ----------
covidImpactPlot <- function(data) {
  df_summary <- data %>%
    group_by(year) %>%
    summarise(underreporting = mean(e_inc_100k - c_newinc_100k, na.rm=TRUE))
  
  p <- ggplot(df_summary, aes(x=year, y=underreporting)) +
    geom_line(color="purple", linewidth=1.2) +
    labs(x="Year", y="Estimated Underreporting") +
    theme_minimal()
  
  ggplotly(p)
}

# ---------- Top 10 TB Incidence ----------
topIncidencePlot <- function(data, selected_year) {
  df <- data %>% 
    filter(year == selected_year) %>%
    mutate(e_inc_100k = ifelse(is.na(e_inc_100k), 0, e_inc_100k)) %>%
    distinct(country_who, .keep_all = TRUE) %>% 
    arrange(desc(e_inc_100k)) %>%
    slice_head(n = 10)
  
  p <- ggplot(df, aes(x = reorder(country_who, e_inc_100k), y = e_inc_100k, fill = e_inc_100k)) +
    geom_col() +
    coord_flip() +
    scale_fill_viridis_c() +
    labs(
      x = "", 
      y = "Incidence per 100k", 
      title = paste("Top 10 TB Incidence —", selected_year)
    ) +
    theme_minimal(base_size = 12)
  
  ggplotly(p)
}


# ---------- Top 10 TB Mortality ----------
topMortalityPlot <- function(data, selected_year) {
  df <- data %>% 
    filter(year == selected_year) %>%
    mutate(e_mort_100k = ifelse(is.na(e_mort_100k), 0, e_mort_100k)) %>%
    distinct(country_who, .keep_all = TRUE) %>% 
    arrange(desc(e_mort_100k)) %>%
    slice_head(n = 10)
  
  p <- ggplot(df, aes(x = reorder(country_who, e_mort_100k), y = e_mort_100k, fill = e_mort_100k)) +
    geom_col() +
    coord_flip() +
    scale_fill_viridis_c(option = "plasma") +
    labs(
      x = "", 
      y = "Mortality per 100k", 
      title = paste("Top 10 TB Mortality —", selected_year)
    ) +
    theme_minimal(base_size = 12)
  
  ggplotly(p)
}



# ------------------- Bubble Plot: TB vs Socioeconomic -------------------
render_bubble_plot <- function(data, x_var, y_var, year_selected, log_x = TRUE, log_y = TRUE) {
  df <- data %>%
    filter(year == year_selected,
           !is.na(.data[[x_var]]),
           !is.na(.data[[y_var]]),
           !is.na(SP.POP.TOTL))
  
  p <- ggplot(df, aes(
    x = .data[[x_var]],
    y = .data[[y_var]],
    size = SP.POP.TOTL,
    color = .data[[y_var]],
    text = paste0(
      "Country: ", country_who, "<br>",
      x_var, ": ", round(.data[[x_var]], 2), "<br>",
      y_var, ": ", round(.data[[y_var]], 2), "<br>",
      "Population: ", round(SP.POP.TOTL/1e6, 2), "M"
    )
  )) +
    geom_point(alpha = 0.7) +
    scale_size_continuous(range = c(5, 25), guide = "none") +
    scale_color_viridis_c(option = "C") +
    labs(x = x_var, y = y_var, color = y_var) +
    theme_minimal(base_size = 14)
  
  if (log_x) p <- p + scale_x_log10()
  if (log_y) p <- p + scale_y_log10()
  
  ggplotly(p, tooltip = "text")
}

# ------------------- MDR-TB Trend -------------------
render_mdr_trend <- function(data) {
  df <- data %>%
    group_by(year) %>%
    summarize(mdr = mean(e_rr_pct_new, na.rm = TRUE))
  
  p <- ggplot(df, aes(x = year, y = mdr)) +
    geom_line(color = "orange", linewidth = 1.2) +
    geom_point(color = "orange") +
    labs(x = "Year", y = "MDR-TB % (New Cases)") +
    theme_minimal()
  
  ggplotly(p)
}

# ------------------- Global Underreporting -------------------
render_global_underreporting <- function(data){
  df <- data %>%
    group_by(year) %>%
    summarise(
      estimated_inc = sum(e_inc_100k, na.rm = TRUE),
      reported = sum(c_newinc_100k, na.rm = TRUE)
    ) %>%
    tidyr::pivot_longer(
      cols = c(estimated_inc, reported),
      names_to = "metric",
      values_to = "value"
    )
  
  p <- ggplot(df, aes(
    x = year,
    y = value,
    color = metric,
    text = paste0(
      "Year: ", year, "<br>",
      "Metric: ", ifelse(metric == "estimated_inc", "Estimated Incidence", "Reported Notifications"), "<br>",
      "Value: ", round(value, 1), " per 100k"
    )
  )) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(
      values = c("estimated_inc" = "orange", "reported" = "blue"),
      labels = c("Estimated Incidence", "Reported Notifications")
    ) +
    labs(
      x = "Year",
      y = "TB Cases per 100k",
      color = ""
    ) +
    theme_minimal(base_size = 14)
  
  ggplotly(p, tooltip = "text")
}

#This correlation heatmap was later moved to the Statistical Summary Tab

# ---------- Correlation ----------
correlationHeatmap <- function(data){
  # Only numeric columns that exist in the dataset
  vars <- names(all)[names(all) %in% colnames(data)]
  cor_data <- data %>% select(all_of(vars)) %>% select(where(is.numeric))
  
  if(ncol(cor_data) > 0){
    # Compute correlation matrix safely even with NAs
    corr_mat <- round(cor(cor_data, use = "pairwise.complete.obs"), 2)
    
    # Friendly labels
    col_labels <- all[colnames(corr_mat)]
    row_labels <- all[rownames(corr_mat)]
    
    plot_ly(
      x = col_labels,
      y = row_labels,
      z = corr_mat,
      type = "heatmap",
      colorscale = "Viridis",
      showscale = TRUE
    ) %>%
      layout(
        title = "Correlation Heatmap: TB Metrics vs Socioeconomic Indicators",
        xaxis = list(tickangle = -45),
        yaxis = list(tickangle = 0)
      )
    
  } else {
    # Empty plot if no numeric data
    plot_ly() %>%
      layout(
        title = "No numeric data available for correlation heatmap"
      )
  }
}



