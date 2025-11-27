# =================Load Global Variables and Data =========================
source("global.R")

# ==========================DASHBOARD PAGE STRUCTURE ========================
ui <- dashboardPage(
  
  # -------------------- HEADER --------------------
  dashboardHeader(title = "Global Tuberculosis Dashboard"),
  
  # -------------------- SIDEBAR --------------------
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "main"),
      menuItem("Research Questions", tabName = "research", icon = icon("lightbulb")),
      menuItem("Statistical Summary", tabName = "stats", icon = icon("chart-bar")),
      menuItem("Trend Analysis", tabName = "trend", icon = icon("chart-line")),
      menuItem("Socioeconomic Correlations", tabName = "socioeconomic", icon = icon("globe")),
      menuItem("Drug Resistance & Treatment", tabName = "drugresistance", icon = icon("medkit")),
      menuItem("COVID-19 Impact", tabName = "covid", icon = icon("virus")),
      menuItem("Glossary", tabName = "glossary", icon = icon("book")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  # -------------------- BODY --------------------
  dashboardBody(
    tabItems(
      
      # =================== MAIN / HOME TAB =================
      tabItem(tabName = "main",
              
              # ---- VALUE BOXES ----
              fluidRow(
                valueBoxOutput("globalIncidence", width = 3),
                valueBoxOutput("globalMortality", width = 3),
                valueBoxOutput("maxMDRTB", width = 3),
                valueBoxOutput("covidDrop", width = 3)
              ),
              
              # ---- SECTION HEADER ----
              fluidRow(
                column(
                  width = 12,
                  h3("🌍 Comparison over the years", align = "center"),
                  p("Explore which countries have the highest TB burden.", 
                    align = "center", style = "color: #555;")
                )
              ),
              br(),
              
              # ---- TOP COUNTRY PLOTS ----
              fluidRow(
                box(
                  title = "Top 10 — TB Incidence (per 100k)",
                  status = "primary", solidHeader = TRUE, width = 6,
                  sliderInput("year_inc", "Select Year:",
                              min = min(global_tb$year), max = max(global_tb$year),
                              value = min(global_tb$year), step = 1, animate = TRUE),
                  plotlyOutput("top_incidence_plot", height = "350px")
                ),
                box(
                  title = "Top 10 — TB Mortality (per 100k)",
                  status = "danger", solidHeader = TRUE, width = 6,
                  sliderInput("year_mort", "Select Year:",
                              min = min(global_tb$year), max = max(global_tb$year),
                              value = min(global_tb$year), step = 1, animate = TRUE),
                  plotlyOutput("top_mortality_plot", height = "350px")
                )
              ),
              
              # ---- SOCIOECONOMIC BUBBLE PLOT ----
              fluidRow(
                box(
                  width = 12,
                  title = "TB Metrics vs Socioeconomic Indicators over the years",
                  status = "info", solidHeader = TRUE,
                  sliderInput("bubble_year", "Select Year:",
                              min = min(global_tb$year, na.rm = TRUE),
                              max = max(global_tb$year, na.rm = TRUE),
                              value = max(global_tb$year, na.rm = TRUE),
                              step = 1, sep = "", animate = TRUE),
                  selectInput("bubble_x", "X-axis:", 
                              choices = setNames(names(wdi_labels), wdi_labels), 
                              selected = "NY.GDP.PCAP.CD"),
                  selectInput("bubble_y", "Y-axis:", 
                              choices = setNames(names(metric_names), metric_names), 
                              selected = "e_inc_100k"),
                  checkboxInput("bubble_log_x", "Log-scale X-axis", value = TRUE),
                  checkboxInput("bubble_log_y", "Log-scale Y-axis", value = TRUE),
                  plotlyOutput("bubblePlot", height = "400px")
                )
              ),
              
              # ---- GLOBAL TRENDS ----
              fluidRow(
                box(width = 6, title = "MDR-TB Trend", status = "info", solidHeader = TRUE,
                    plotlyOutput("mdrTrendPlot", height = "300px")),
                box(width = 6, title = "Global TB Notifications vs Estimated Incidence", solidHeader = TRUE, status = "danger",
                    plotlyOutput("globalUnderreporting", height = "300px"))
              ),
              
              # ---- COMORBIDITY & ENVIRONMENT ----
              fluidRow(
                box(
                  width = 6,
                  title = "TB vs Comorbidities",
                  status = "warning",
                  solidHeader = TRUE,
                  selectInput("comorb_y", "Select TB Metric:",
                              choices = setNames(names(metric_names), metric_names),
                              selected = "e_inc_100k"),
                  selectInput("comorb_x", "Select Comorbidity:",
                              choices = setNames(names(wdi_disease), wdi_disease),
                              selected = "SH.DYN.AIDS.ZS"),
                  checkboxInput("comorb_log_x", "Log-scale X-axis", value = FALSE),
                  checkboxInput("comorb_log_y", "Log-scale Y-axis", value = TRUE),
                  plotlyOutput("comorbPlot", height = "300px")
                ),
                box(
                  width = 6,
                  title = "TB vs Environmental Factors",
                  status = "success",
                  solidHeader = TRUE,
                  selectInput("env_x", "Select Environmental Factor:",
                              choices = setNames(names(wdi_env), wdi_env),
                              selected = "EN.ATM.PM25.MC.M3"),
                  selectInput("env_y", "Select TB Metric:",
                              choices = setNames(names(metric_names), metric_names),
                              selected = "e_inc_100k"),
                  checkboxInput("env_log_x", "Log-scale X-axis", value = FALSE),
                  checkboxInput("env_log_y", "Log-scale Y-axis", value = TRUE),
                  plotlyOutput("envPlot", height = "300px")
                )
              )
      ),
      
      # ================= Analysis and Research Questions Page =================
      tabItem(tabName = "research",
              fluidRow(
                box(width = 12, title = "Research Questions & Key Insights", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    div(style = "height: 600px;",
                        HTML("
                   <h4>1. How has global TB incidence and mortality changed over the years?</h4>
                   <p><b>Insight:</b> TB incidence as well as mortality has generally declined globally but at different rates in different countries.</p>
                   
                   <h4>2. How does TB mortality correlate with incidence?</h4>
                   <p><b>Insight:</b> Mortality due to TB is positively correlated with incidence trend as expected. Since with decrement in number of cases we also see decrement in number of deaths across the world.</p>
                   
                   <h4>3. How is TB burden related to socioeconomic indicators like GDP or poverty?</h4>
                   <p><b>Insight:</b> Countries with higher GDP per capita tend to invest more in healthcare facilities thus have lower incidence of TB cases.</p>
                   
                   <h4>4. Are people diagnosed with MDR-TB cases likely to be diagnosed with MDR-TB again?</h4>
                   <p><b>Insight:</b> People who are diagnosed with MDR-TB tend to have a higher chance of being diagnosed with it again. </p>
                   
                   <h4>5. How did COVID-19 impact TB occurence?</h4>
                   <p><b>Insight:</b> During the COVID 19 pandemic period TB cases showed a increment in cases during the year 2020 to 2021.</p>
                   
                   <h4>6. How is TB occurence correlated with smoking, diabetes or HIV occurence?</h4>
                   <p><b>Insight:</b> People having HIV are more likely to suffer from TB. But TB occurence shows almost zero correlation with smoking and slightly negative correlation with diabetes.</p>
                   
                   <h4>7. Compare MDR-TB% new VS GDP(USD) </h4>
                   <p><b>Insight:</b> India shows a gradual decline in MDR-TB percentage among new TB cases as GDP per capita increases. Although the decline is visible, it isn’t very steep. This suggests that even with economic growth, the drop in MDR-TB cases has been gradual.</p>
                   
                   <h4>8. Compare MDR-TB trend analysis in South East asian countries vs USA</h4>
                   <p><b>Insight:</b> In most of the South East asian countries the occurence of MDR-TB is more or less constant over the years. However USA has shown a decrement in rate of occurence of new cases. But Brazil has a sudden increment of cases in pre covid time. However Canada has a steady increment throughout</p>        
                             ")
                    )
                )
              )
      ),
      
      # ==========================STATISTICAL SUMMARY TAB ==========================
      tabItem(tabName = "stats",
              
              # ---- SECTION TITLE ----
              fluidRow(
                column(
                  width = 12,
                  h3("Compare Tuberculosis Statistics Between Countries", 
                     align = "center", style = "color: #555;")
                )),
              br(),
              
              # ---- INPUT CONTROLS ----
              fluidRow(
                # Country 1 selector (defaults to India)
                box(width = 4, status = "primary", solidHeader = TRUE,
                    selectInput("country1", "Select Country 1:",
                                choices = sort(unique(global_tb$country_who)),
                                selected = "India")),
                
                # Country 2 multi-selection + Clear button
                box(width = 4, status = "primary", solidHeader = TRUE,
                    selectInput("country2", "Select Country 2 (one or more):",
                                choices = sort(unique(global_tb$country_who)),
                                selected = NULL, multiple = TRUE),
                    actionButton("clearCountries", "Clear Selection", icon = icon("eraser"),
                                 class = "btn-warning")),
                
                # Metric selection
                box(width = 4, status = "primary", solidHeader = TRUE,
                    selectInput("stats_metric", "Select Metric:",
                                choices = setNames(names(metric_names), metric_names),
                                selected = "e_inc_100k"))
              ),
              
              # ---- SUMMARY TABLE ----
              fluidRow(
                box(title = "Summary Statistics", width = 12, status = "info",
                    solidHeader = TRUE, tableOutput("summaryTable"))
              ),
              
              # ---- COMPARISON PLOTS ----
              fluidRow(
                box(title = "Density Comparison", width = 6, status = "success",
                    solidHeader = TRUE, plotlyOutput("densityPlot")),
                box(title = "Boxplot Comparison", width = 6, status = "success",
                    solidHeader = TRUE, plotlyOutput("boxComparePlot"))
              ),
              
              # ---- CORRELATION HEATMAP ----
              fluidRow(
                box(title = "Correlation Heatmap", width = 12, status = "warning",
                    solidHeader = TRUE, plotlyOutput("corrPlot"))
              )
      ),
      
      
      # ================= Trend Analysis =================
      tabItem(tabName = "trend",
              fluidRow(
                valueBoxOutput("trendMax", width = 2),
                valueBoxOutput("trendMin", width = 2),
                valueBoxOutput("trendLatest", width = 2)
              ),
              fluidRow(
                box(width = 6,
                    selectInput("trend_country", "Select Country(s):",
                                choices = sort(unique(global_tb$country_who)),
                                selected = "India",
                                multiple = TRUE,
                                selectize = TRUE)),
                box(width = 6,
                    selectInput("trend_metric", "Select Metric:",
                                choices = setNames(names(metric_names), metric_names),
                                selected = "e_inc_100k"))
              ),
              fluidRow(
                box(width = 12,
                    actionButton("resetTrendPlot", "Reset to Default"),
                    plotlyOutput("trendPlot", height = "400px"))
              )
      ),
      
      # ================= Socioeconomic Correlations =================
      tabItem(tabName = "socioeconomic",
              fluidRow(
                box(width = 6, 
                    selectInput("socio_country", "Select Country(s):",
                                choices = sort(unique(global_tb$country_who)),
                                selected = "India",
                                multiple = TRUE,
                                selectize = TRUE)),
                box(width = 3, 
                    selectInput("x_var", "X-axis:",
                                choices = setNames(names(wdi_labels), wdi_labels),
                                selected = "NY.GDP.PCAP.CD")),
                box(width = 3, 
                    selectInput("y_vars", "Y-axis Metrics:", 
                                choices = setNames(names(metric_names), metric_names),
                                selected = "e_inc_100k",
                                multiple = TRUE,
                                selectize = TRUE))
              ),
              fluidRow(
                box(width = 12,
                    actionButton("resetSocioPlot", "Reset to Default"),
                    plotlyOutput("socioPlot", height = "400px"))
              )
      ),
      
      # ================= Drug Resistance & Treatment =================
      tabItem(tabName = "drugresistance",
              fluidRow(
                box(width = 6, 
                    selectInput("rr_country", "Select Country(s):",
                                choices = sort(unique(global_tb$country_who)),
                                selected = "India",
                                multiple = TRUE,
                                selectize = TRUE)),
                box(width = 6, 
                    selectInput("rr_metric", "Select Drug Resistance Metric:",
                                choices = setNames(names(rr_labels), rr_labels),
                                selected = "e_rr_pct_new"))
              ),
              fluidRow(
                box(width = 12,
                    actionButton("resetRRPlot", "Reset to Default"),
                    plotlyOutput("rrPlot", height = "400px"))
              )
      ),
      
      # ================= COVID Impact =================
      tabItem(tabName = "covid",
              fluidRow(
                box(width = 6, 
                    selectInput("covid_country", "Select Country(s):",
                                choices = sort(unique(global_tb$country_who)),
                                selected = "India",
                                multiple = TRUE,
                                selectize = TRUE))
              ),
              fluidRow(
                box(width = 12,
                    actionButton("resetCovidPlot", "Reset to Default"),
                    plotlyOutput("covidPlot", height = "400px"))
              )
      ),
      
      # ================= Glossary =================
      tabItem(tabName = "glossary",
              fluidRow(
                box(title = "Glossary / Key TB Terms", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, 
                    height = "700px",
                    div(style = "height: 700px;",
                        HTML(
                          "
                          <h4>1. TB Incidence</h4>
                          <p>Number of new active tuberculosis cases per 100,000 population in a year.</p>
                          <h4>2. TB Mortality</h4>
                          <p>Number of deaths due to TB per 100,000 population in a year.</p>
                          <h4>3. TB Notifications</h4>
                          <p>Number of cases officially reported to health authorities.</p>
                          <h4>4. MDR-TB</h4>
                          <p>TB resistant to both isoniazid and rifampicin.</p>
                          <h4>5. RR-TB</h4>
                          <p>TB resistant to rifampicin. All MDR-TB cases are RR-TB, but not all RR-TB cases are MDR-TB.</p>
                          <h4>6. Latent TB Infection</h4>
                          <p>Person carries TB bacteria but shows no symptoms and is not contagious.</p>
                          <h4>7. Treatment Success Rate</h4>
                          <p>Percentage of TB patients who successfully complete treatment.</p>
                          <h4>8. Underreporting / Missed Cases</h4>
                          <p>Difference between estimated incidence and reported notifications.</p>
                          <h4>9. TB Risk Factors</h4>
                          <p>HIV, diabetes, malnutrition, smoking, urbanization, and crowding.</p>
                          <h4>10. COVID-19 Impact</h4>
                          <p>Healthcare disruptions reduced TB notifications during lockdowns.</p>
                          "
                        )
                    )
                )
              )
      ),
      
      # ================= About =================
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "About This Project", width = 12, status = "primary", solidHeader = TRUE,
                  
                  h3("Project: Global Tuberculosis Analysis Dashboard"),
                  
                  p("This interactive dashboard provides a comprehensive analysis of Tuberculosis (TB) trends globally and regionally. 
    It integrates data from the WHO, World Bank, and other open-access sources to visualize TB incidence, mortality, drug resistance, socio-economic correlates, 
    and the impact of COVID-19 disruptions."),
                  
                  h4("Problem Statement"),
                  p("TB remains a leading infectious disease worldwide. Despite global and national control programs, TB persists, particularly in regions with poverty, undernutrition, and limited health infrastructure. 
    This dashboard explores TB trends, multidrug resistance, socio-economic factors, and pandemic-related disruptions."),
                  
                  h4("Objectives"),
                  tags$ul(
                    tags$li("Track global and country-level TB incidence, mortality, and notifications."),
                    tags$li("Explore correlations of TB burden with socioeconomic indicators like GDP, poverty, health expenditure, and urbanization."),
                    tags$li("Analyze trends in MDR-TB and RR-TB and their treatment outcomes."),
                    tags$li("Quantify COVID-19 impacts on TB reporting and notifications."),
                    tags$li("Provide a user-friendly, interactive dashboard for further research and visualization.")
                  ),
                  
                  h4("Data Sources"),
                  tags$ul(
                    tags$li(a("WHO Global TB Programme", href="https://www.who.int/teams/global-tuberculosis-programme/data", target="_blank")),
                    tags$li(a("World Bank WDI", href="https://databank.worldbank.org/source/world-development-indicators", target="_blank"))
                  ),
                  
                  h4("Key References"),
                  tags$ul(
                    tags$li(a("End TB Strategy – WHO", href="https://www.who.int/teams/global-tuberculosis-programme/the-end-tb-strategy", target="_blank")),
                    tags$li(a("Global TB Report 2024", href="https://iris.who.int/server/api/core/bitstreams/7292c91e-ffb0-4cef-ac39-0200f06961ea/content", target="_blank")),
                    tags$li(a("The Social Determinants of Tuberculosis: From Evidence to Action", href="https://ajph.aphapublications.org/doi/full/10.2105/AJPH.2010.199505", target="_blank")),
                    tags$li(a("Sociodemographic factors affecting knowledge levels of tuberculosis patients in New Delhi", href="https://journals.lww.com/jfmpc/fulltext/2024/13110/sociodemographic_factors_affecting_knowledge.60.aspx", target="_blank"))
                  )
                  
                  
                )
              )
      )
      
    )
)
)
