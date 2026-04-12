# scripts/01_build_country_daily.R

library(dplyr)
library(tidyr)

# source("R/io_import.R")

source("../R/io_import.R")

country_daily <- aggregate_jhu_country(
  confirmed_path = "../data_raw/time_series_covid19_confirmed_global.csv",
  deaths_path    = "../data_raw/time_series_covid19_deaths_global.csv",
  recovered_path = "../data_raw/time_series_covid19_recovered_global.csv"
)

saveRDS(country_daily, "../data_processed/covid_country_daily.rds")
message("Saved: data_processed/covid_country_daily.rds")
