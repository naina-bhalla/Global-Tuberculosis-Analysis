# This file contains the scraping-cum-combining code. It extracts the specific excel sheets downloaded from WHO's websites.
# The indicators were handpicked after going through \data\who_data\TB_data_dictionary_2025-09-26.csv file.
# The indicators along with some specific World Bank indicators were combined together to form 1 csv file by the name of /data/who_tb_global.csv
# This final csv file is used further in the project/app.

library(tidyverse)
library(rvest)
library(dplyr)
library(countrycode)
library(WDI)

budget<-read.csv("data/who_data/TB_budget_2025-09-26.csv")
burden<-read.csv("data/who_data/TB_burden_countries_2025-09-26.csv")
rr_burden<-read.csv("data/who_data/MDR_RR_TB_burden_estimates_2025-09-26.csv")

unique(burden$year)

burden_vars <- names(burden)   
budget_vars <- names(budget)
rr_vars     <- names(rr_burden)


who_data <- burden %>%
  select(country, year, g_whoregion, e_pop_num,
         e_inc_100k, e_inc_num,
         e_mort_100k, e_mort_num,
         e_inc_tbhiv_100k, e_inc_tbhiv_num,
         e_mort_tbhiv_100k, e_mort_tbhiv_num,
         cfr, cfr_pct, c_newinc_100k, c_cdr) %>%
  left_join(budget %>%
              select(country, year, budget_tot, cf_tot_sources,
                     budget_lab, budget_tpt, budget_staff, budget_mdrmgt,
                     cf_tot_domestic, cf_tot_gf, cf_tot_usaid),
            by = c("country","year")) %>%
  left_join(rr_burden %>%
              select(country, year, e_rr_pct_new, e_rr_pct_ret,
                     e_inc_rr_num, e_rr_in_notified_labconf_pulm),
            by = c("country","year"))


indicators <- c(
  "NY.GDP.PCAP.CD",     # GDP per capita
  "SP.POP.TOTL",        # Population
  "SP.URB.TOTL.IN.ZS",  # Urban %
  "SH.XPD.CHEX.PC.CD",  # Health exp per capita
  "SI.POV.DDAY",        # Poverty %
  "SE.ADT.LITR.ZS",     # Literacy
  "SH.DYN.AIDS.ZS",     # HIV prevalence
  "SH.STA.DIAB.ZS",     # Diabetes prevalence
  "SH.PRV.SMOK"         # Smoking prevalence
)

wdi_data <- WDI(
  country = "all",
  indicator = indicators,
  start = 2000,
  end = 2023
)

# Add environmental indicators
env_indicators <- c(
  "EN.ATM.PM25.MC.M3",   # PM2.5 mean exposure
  "EN.ATM.PM25.MC.ZS",   # % above WHO guideline
  "EG.USE.SOLID.FO.ZS"   # % population using solid fuels
)

env_data <- WDI(
  country = "all",
  indicator = env_indicators,
  start = 2000,
  end = 2023
)

wdi_full <- bind_rows(
  wdi_data %>% mutate(type = "socioeconomic"),
  env_data %>% mutate(type = "environmental")
)


# Add iso2c and iso3c codes
who_data <- who_data %>%
  mutate(
    iso2c = countrycode(country, "country.name", "iso2c"),
    iso3c = countrycode(country, "country.name", "iso3c")
  )


# Merge
full_data <- who_data %>%
  left_join(wdi_full, by = c("iso3c", "year")) %>%
  rename(country_who = country.x, country_wdi = country.y) %>%
  filter(!is.na(country_who) & !is.na(year))


# Save full_data as CSV
write.csv(full_data, "data/who_tb_global.csv", row.names = FALSE)


