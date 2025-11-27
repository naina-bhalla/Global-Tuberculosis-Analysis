# This contains the list of libraries and the common labels and their units used across the app. This also loads the dataset.

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(viridis)

# Load dataset
global_tb <- read.csv("data/who_tb_global.csv")

# ====================== Mapping Labels ======================
metric_names <- c(
  "e_inc_100k" = "TB Incidence",
  "e_mort_100k" = "TB Mortality",
  "c_newinc_100k" = "Notifications",
  "e_rr_pct_new" = "MDR-TB % New",
  "e_rr_pct_ret" = "MDR-TB % Retreatment",
  "e_inc_rr_num" = "Total RR-TB Cases"
)

x_labels <- c(
  "NY.GDP.PCAP.CD" = "GDP per capita",
  "SH.XPD.CHEX.PC.CD" = "Health expenditure per capita",
  "SI.POV.DDAY" = "Poverty %",
  "SP.URB.TOTL.IN.ZS" = "Urban population %"
)

rr_labels <- c(
  "e_rr_pct_new" = "MDR-TB Percentage (New Cases)",
  "e_rr_pct_ret" = "MDR-TB Percentage (Retreatment Cases)",
  "e_inc_rr_num" = "Total Rifampicin-Resistant TB Cases"
)

wdi_labels <- c(
  "NY.GDP.PCAP.CD" = "GDP per capita (USD)",
  "SP.POP.TOTL" = "Total population",
  "SP.URB.TOTL.IN.ZS" = "Urban population (%)",
  "SH.XPD.CHEX.PC.CD" = "Health expenditure per capita (USD)",
  "SI.POV.DDAY" = "Poverty rate (%)",
  "SE.ADT.LITR.ZS" = "Adult literacy rate (%)"
)

wdi_disease <- c(
  "SH.DYN.AIDS.ZS" = "HIV prevalence (%)",
  "SH.STA.DIAB.ZS" = "Diabetes prevalence (%)",
  "SH.PRV.SMOK" = "Smoking prevalence (%)"
)

wdi_env <- c(
  "EN.ATM.PM25.MC.M3" = "PM2.5 mean exposure (µg/m³)",
  "EN.ATM.PM25.MC.ZS" = "% population > WHO PM2.5 guideline"
)

all <- c(
  "e_inc_100k" = "TB Incidence",
  "e_mort_100k" = "TB Mortality",
  "c_newinc_100k" = "Notifications",
  "e_rr_pct_new" = "MDR-TB % New",
  "e_rr_pct_ret" = "MDR-TB % Retreatment",
  "e_inc_rr_num" = "Total RR-TB Cases",
  "NY.GDP.PCAP.CD" = "GDP per capita (USD)",
  "SP.POP.TOTL" = "Total population",
  "SP.URB.TOTL.IN.ZS" = "Urban population (%)",
  "SH.XPD.CHEX.PC.CD" = "Health expenditure per capita (USD)",
  "SI.POV.DDAY" = "Poverty rate (%)",
  "SE.ADT.LITR.ZS" = "Adult literacy rate (%)",
  "SH.DYN.AIDS.ZS" = "HIV prevalence (%)",
  "SH.STA.DIAB.ZS" = "Diabetes prevalence (%)",
  "SH.PRV.SMOK" = "Smoking prevalence (%)",
  "EN.ATM.PM25.MC.M3" = "PM2.5 mean exposure (µg/m³)",
  "EN.ATM.PM25.MC.ZS" = "% population > WHO PM2.5 guideline"
)

# ====================== Units for Metrics ======================
metric_units <- c(
  "e_inc_100k" = "per 100,000 population",
  "e_mort_100k" = "per 100,000 population",
  "c_newinc_100k" = "per 100,000 population",
  "e_rr_pct_new" = "%",
  "e_rr_pct_ret" = "%",
  "e_inc_rr_num" = "per 100,000 population",
  "e_mort_exc_tbhiv_100k" = "per 100,000 population",
  "e_mort_tbhiv_100k" = "per 100,000 population",
  "e_prev_100k" = "per 100,000 population",
  "e_pop_num" = "persons",
  "underreporting" = "per 100,000 population"
)

# ====================== Units for WDI (World Bank Indicators) ======================
wdi_units <- c(
  "NY.GDP.PCAP.CD" = "current US$",
  "SP.POP.TOTL" = "people",
  "SH.XPD.CHEX.PC.CD" = "current US$ per capita",
  "SE.XPD.TOTL.GD.ZS" = "% of GDP",
  "SL.UEM.TOTL.ZS" = "%",
  "SH.H2O.BASW.ZS" = "% of population",
  "SP.DYN.LE00.IN" = "years",
  "SP.POP.GROW" = "%",
  "SH.DYN.MORT" = "per 1,000 live births",
  "EN.ATM.CO2E.PC" = "metric tons per capita",
  "EN.ATM.PM25.MC.M3" = "µg/m³",
  "SI.POV.DDAY" = "% of population below poverty line",
  "SP.URB.TOTL.IN.ZS" = "% of total population",
  "SE.ADT.LITR.ZS" = "% of adults",
  "SH.DYN.AIDS.ZS" = "% of population aged 15-49",
  "SH.STA.DIAB.ZS" = "% of population aged 20-79",
  "SH.PRV.SMOK" = "% of adults",
  "EN.ATM.PM25.MC.ZS" = "% of population"
)

# ====================== Units for Drug Resistance & Treatment ======================
rr_units <- c(
  "e_rr_pct_new" = "%",
  "e_rr_pct_ret" = "%",
  "e_mdr_pct_new" = "%",
  "e_mdr_pct_ret" = "%",
  "e_rr_num" = "cases",
  "e_rr_num_ret" = "cases",
  "e_inc_rr_num" = "cases"
)
