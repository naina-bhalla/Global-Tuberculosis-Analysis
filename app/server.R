# this file contains the server code for the app. It directly sources some plots from other files.
# This also has the default input values for the dropdowns and plots for some other tabs.

source("global.R")
source("global_plots.R")
source("home_plots.R")

server <- function(input, output, session) {
  # Store plot data reactively
  plot_data <- reactiveValues(
    trend = NULL,
    socio = NULL,
    rr = NULL,
    covid = NULL
  )
  
  # Default input values
  default_values <- list(
    trend_country = "India",
    trend_metric = "e_inc_100k",
    socio_country = "India",
    x_var = "NY.GDP.PCAP.CD",
    y_vars = "e_inc_100k",
    rr_country = "India",
    rr_metric = "e_rr_pct_new",
    covid_country = "India"
  )
  
  # ---------- Clear & Reset Handlers ----------
  
  observeEvent(input$resetTrendPlot, {
    updateSelectInput(session, "trend_country", selected = default_values$trend_country)
    updateSelectInput(session, "trend_metric", selected = default_values$trend_metric)
  })
  
  observeEvent(input$resetSocioPlot, {
    updateSelectInput(session, "socio_country", selected = default_values$socio_country)
    updateSelectInput(session, "x_var", selected = default_values$x_var)
    updateSelectInput(session, "y_vars", selected = default_values$y_vars)
  })
  
  observeEvent(input$resetRRPlot, {
    updateSelectInput(session, "rr_country", selected = default_values$rr_country)
    updateSelectInput(session, "rr_metric", selected = default_values$rr_metric)
  })
  
  observeEvent(input$resetCovidPlot, {
    updateSelectInput(session, "covid_country", selected = default_values$covid_country)
  })
  
    
  # ---------- Statistical Summary----------
  
  
  # Clear selections
  observeEvent(input$clearCountries, {
    updateSelectInput(session, "country2", selected = character(0))
  })
  
  # Filtered data
  filtered_data <- reactive({
    req(input$stats_metric)
    countries <- c(input$country1, input$country2) 
    countries <- countries[countries != ""] # remove empty
    validate(need(length(countries) > 0, "Please select at least one country."))
    global_tb %>% filter(country_who %in% countries)
  })
  
  # ----- Density Plot -----
  output$densityPlot <- renderPlotly({
    df <- filtered_data()
    metric <- input$stats_metric
    unit <- metric_units[[metric]] %||% ""  # safely handle missing unit names
    
    # Create the density plot
    p <- ggplot(df, aes_string(x = metric, fill = "country_who", color = "country_who")) +
      geom_density(alpha = 0.4, adjust = 1.2, linewidth = 1) +
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Density Comparison of", metric_names[[metric]], 
                      if (unit != "") paste0("(", unit, ")") else ""),
        x = paste0(metric_names[[metric]], if (unit != "") paste0(" (", unit, ")") else ""),
        y = "Density",
        fill = "Country",
        color = "Country"
      )
    
    ggplotly(p) %>%
      layout(title = list(text = paste(metric_names[[metric]], 
                                       if (unit != "") paste0(" (", unit, ")") else "")))
  })
  
  
  # ----- Boxplot Comparison -----
  output$boxComparePlot <- renderPlotly({
    df <- filtered_data()
    metric <- input$stats_metric
    
    p <- ggplot(df, aes(x = country_who, y = .data[[metric]], fill = country_who)) +
      geom_boxplot(alpha = 0.6) +
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Boxplot Comparison of", metric_names[[metric]]),
        x = "Country", y = metric_names[[metric]], fill = "Country"
      )
    
    ggplotly(p)
  })
  
  # ----- Summary Table -----
  output$summaryTable <- renderTable({
    df <- filtered_data() 
    metric <- input$stats_metric
    unit <- metric_units[[metric]]
    years <- paste0(min(df$year, na.rm = TRUE), "–", max(df$year, na.rm = TRUE))
    
    summary_df <- df %>%
      group_by(country_who) %>%
      summarise(
        Mean = round(mean(.data[[metric]], na.rm = TRUE), 2),
        Median = round(median(.data[[metric]], na.rm = TRUE), 2),
        Variance = round(var(.data[[metric]], na.rm = TRUE), 2),
        SD = round(sd(.data[[metric]], na.rm = TRUE), 2),
        Min = round(min(.data[[metric]], na.rm = TRUE), 2),
        Max = round(max(.data[[metric]], na.rm = TRUE), 2)
      ) %>%
      rename(Country = country_who)
    
    colnames(summary_df)[-1] <- paste0(colnames(summary_df)[-1], " (", unit, ")")
    
    summary_df %>%
      mutate(Year_Range = years) %>%
      relocate(Year_Range, .after = Country)
  })
  
  

  
  
  #---------- Correlation Heatmap ----------
  output$corrPlot <- renderPlotly({
    correlationHeatmap(global_tb)
  })

  
  # ---------- Trend Analysis ----------
  output$trendPlot <- renderPlotly({
    req(input$trend_country)
    
    df <- global_tb %>%
      filter(country_who %in% input$trend_country) %>%
      filter(!is.na(.data[[input$trend_metric]]))
    
    p <- ggplot(df, aes(x = year, y = .data[[input$trend_metric]], color = country_who)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      labs(title = paste(metric_names[input$trend_metric], "Trend"),
           x = "Year", y = paste(metric_names[[input$trend_metric]], "(", metric_units[[input$trend_metric]], ")"), color = "Country") +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = c("x", "y", "color")) %>%
      layout(title = paste(metric_names[[input$trend_metric]], "(", metric_units[[input$trend_metric]], ")"))
  })
  
  

  
  # ---------- Socioeconomic Correlations ----------
  output$socioPlot <- renderPlotly({
    req(input$socio_country, input$x_var, input$y_vars)
    
    df <- global_tb %>%
      filter(country_who %in% input$socio_country) %>%
      select(country_who, x = .data[[input$x_var]], all_of(input$y_vars)) %>%
      tidyr::pivot_longer(cols = all_of(input$y_vars),
                          names_to = "metric",
                          values_to = "value") %>%
      filter(!is.na(x), !is.na(value)) %>%
      mutate(metric = factor(metric, levels = input$y_vars, labels = metric_names[input$y_vars]))
    
    p <- ggplot(df, aes(x = x, y = value, color = country_who, linetype = metric)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        title = paste("Metrics vs", wdi_labels[input$x_var]),
        x = paste(wdi_labels[input$x_var], "(", wdi_units[[input$x_var]], ")"),
        y = paste("Value (", metric_units[[input$y_vars[1]]], ")"),
        color = "Country",
        linetype = "Metric"
      ) +
      theme_minimal(base_size = 14)
    
    ggplotly(p) 
    
    
  })
  

  
  # ---------- Drug Resistance & Treatment ----------
  checkboxInput("show_rr_percent", "Show as percentage", value = TRUE)
  
  output$rrPlot <- renderPlotly({
    req(input$rr_country)
    
    df <- global_tb %>%
      filter(country_who %in% input$rr_country) %>%
      filter(!is.na(.data[[input$rr_metric]]))

    y_label <- if (grepl("pct", input$rr_metric)) "%" else rr_units[[input$rr_metric]]
    
    p <- ggplot(df, aes(x = year, y = .data[[input$rr_metric]], color = country_who)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      labs(title = paste(rr_labels[input$rr_metric], "Trend"),
           x = "Year", y = y_label, color = "Country") +
      theme_minimal(base_size = 14)
    
    ggplotly(p)
  })
  
  
  # ---------- COVID-19 Impact ----------
  
  output$covidPlot <- renderPlotly({
    req(input$covid_country)
    
    df <- global_tb %>%
      filter(country_who %in% input$covid_country) %>%
      filter(!is.na(e_inc_100k) & !is.na(c_newinc_100k)) %>%
      mutate(underreporting = e_inc_100k - c_newinc_100k)
    
    covid_df <- df %>%
      select(year, country_who, e_inc_100k, c_newinc_100k, underreporting) %>%
      tidyr::pivot_longer(
        cols = c(e_inc_100k, c_newinc_100k, underreporting),
        names_to = "metric",
        values_to = "value"
      ) %>%
      mutate(
        metric = factor(metric,
                        levels = c("e_inc_100k", "c_newinc_100k", "underreporting"),
                        labels = c("Estimated Incidence", "Notifications", "Estimated Underreporting"))
      )
    
    linetypes <- c("Estimated Incidence" = "solid",
                   "Notifications" = "dotted",
                   "Estimated Underreporting" = "dash")
    
    # Generate distinct colors for countries
    n_countries <- length(input$covid_country)
    colors <- viridis(n_countries)
    names(colors) <- input$covid_country
    
    p <- ggplot(covid_df, aes(x = year, y = value, color = country_who, linetype = metric)) +
      geom_line(linewidth = 1.2) +
      scale_linetype_manual(values = linetypes) +
      scale_color_manual(values = colors) +
      labs(title = "COVID-19 Impact on TB Notifications",
           x = "Year", y = "Cases per 100k",
           color = "Country", linetype = "Metric") +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = c("x", "y", "color", "linetype"))
  })
  
  ####### Home Page #######
    
    # ---------- Value Boxes ----------
    output$globalIncidence <- renderValueBox({
      val <- round(mean(global_tb$e_inc_100k, na.rm = TRUE), 1)
      valueBox(val, "Avg TB Incidence (per 100k)", icon = icon("virus"), color = "blue")
    })
    
    output$globalMortality <- renderValueBox({
      val <- round(mean(global_tb$e_mort_100k, na.rm = TRUE), 1)
      valueBox(val, "Avg TB Mortality (per 100k)", icon = icon("skull-crossbones"), color = "red")
    })
    
    output$maxMDRTB <- renderValueBox({
      val <- max(global_tb$e_rr_pct_new, na.rm = TRUE)
      valueBox(val, "Max MDR-TB % (New Cases)", icon = icon("exclamation-triangle"), color = "yellow")
    })
    
    output$covidDrop <- renderValueBox({
      val <- max(global_tb$e_inc_100k - global_tb$c_newinc_100k, na.rm = TRUE)
      valueBox(val, "Max Underreporting per 100k", icon = icon("calendar-times"), color = "purple")
    })
    
    # ---------- Top 10 Plots ----------
    output$top_incidence_plot <- renderPlotly({
      topIncidencePlot(global_tb, input$year_inc)
    })
    
    output$top_mortality_plot <- renderPlotly({
      topMortalityPlot(global_tb, input$year_mort)
    })
    
    # ---------- Bubble Plot ----------
    output$bubblePlot <- renderPlotly({
      render_bubble_plot(global_tb, input$bubble_x, input$bubble_y,
                         input$bubble_year, input$bubble_log_x, input$bubble_log_y)
    })
    
    # ---------- MDR-TB Trend ----------
    output$mdrTrendPlot <- renderPlotly({
      render_mdr_trend(global_tb)
    })
    
    # ---------- Global Underreporting ----------
    output$globalUnderreporting <- renderPlotly({
      render_global_underreporting(global_tb)
    })
    
    
    
    # TB vs Comorbidities
    output$comorbPlot <- renderPlotly({
      req(input$comorb_x, input$comorb_y)
      
      df <- global_tb %>%
        filter(!is.na(.data[[input$comorb_x]]),
               !is.na(.data[[input$comorb_y]]))
      
      # Filter out invalid log values if log scale is selected
      if (input$comorb_log_x) df <- df %>% filter(.data[[input$comorb_x]] > 0)
      if (input$comorb_log_y) df <- df %>% filter(.data[[input$comorb_y]] > 0)
      
      p <- ggplot(df, aes(
        x = .data[[input$comorb_x]],
        y = .data[[input$comorb_y]],
        color = .data[[input$comorb_y]],
        text = paste0(
          "Country: ", country_who, "<br>",
          wdi_disease[input$comorb_x], ": ", round(.data[[input$comorb_x]], 2), "<br>",
          metric_names[input$comorb_y], ": ", round(.data[[input$comorb_y]], 2)
        )
      )) +
        geom_point(size = 3, alpha = 0.7) +
        scale_color_viridis_c(option = "C") +
        labs(
          x = wdi_disease[input$comorb_x],
          y = metric_names[input$comorb_y],
          color = metric_names[input$comorb_y]
        ) +
        theme_minimal(base_size = 14)
      
      if (input$comorb_log_x) p <- p + scale_x_log10(oob = scales::squish_infinite)
      if (input$comorb_log_y) p <- p + scale_y_log10(oob = scales::squish_infinite)
      
      ggplotly(p, tooltip = "text")
    })
    
    
    #TB vs Enbvironmental Factors
    output$envPlot <- renderPlotly({
      req(input$env_x, input$env_y)
      
      df <- global_tb %>%
        filter(!is.na(.data[[input$env_x]]),
               !is.na(.data[[input$env_y]]))
      
      # Filter out invalid log values if log scale is selected
      if (input$env_log_x) df <- df %>% filter(.data[[input$env_x]] > 0)
      if (input$env_log_y) df <- df %>% filter(.data[[input$env_y]] > 0)
      
      p <- ggplot(df, aes(
        x = .data[[input$env_x]],
        y = .data[[input$env_y]],
        color = .data[[input$env_y]],
        text = paste0(
          "Country: ", country_who, "<br>",
          wdi_env[input$env_x], ": ", round(.data[[input$env_x]], 2), "<br>",
          metric_names[input$env_y], ": ", round(.data[[input$env_y]], 2)
        )
      )) +
        geom_point(size = 3, alpha = 0.7) +
        scale_color_viridis_c(option = "C") +
        labs(
          x = wdi_env[input$env_x],
          y = metric_names[input$env_y],
          color = metric_names[input$env_y]
        ) +
        theme_minimal(base_size = 14)
      
      if (input$env_log_x) p <- p + scale_x_log10(oob = scales::squish_infinite)
      if (input$env_log_y) p <- p + scale_y_log10(oob = scales::squish_infinite)
      
      ggplotly(p, tooltip = "text")
    })
    
    
    
}
  

