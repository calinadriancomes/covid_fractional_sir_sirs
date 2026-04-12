# scripts/02_build_model_input.R

library(dplyr)

source("../R/utils.R")
source("../R/preprocess_covid.R")

country_daily <- readRDS("../data_processed/covid_country_daily.rds")

# opțional: înlocuiești ulterior cu un fișier real de populații pe țări
population_table <- data.frame(
  country = c("Romania", "Italy", "US"),
  population = c(19237691, 59554023, 331002651),
  stringsAsFactors = FALSE
)

model_input <- build_all_countries_model_input(
  country_df = country_daily,
  population_table = population_table,
  fallback_population = 1e7
)

model_input <- add_standard_ml_features(model_input)

saveRDS(model_input, "../data_processed/covid_country_model_input.rds")
message("Saved: data_processed/covid_country_model_input.rds")
