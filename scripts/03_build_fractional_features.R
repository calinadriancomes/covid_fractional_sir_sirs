# scripts/03_build_fractional_features.R

library(dplyr)

source("../R/fractional_features.R")

model_input <- readRDS("../data_processed/covid_country_model_input.rds")

model_frac <- add_fractional_features(
  df = model_input,
  alphas = c(0.2, 0.4, 0.6, 0.8),
  k_max = 30
)

saveRDS(model_frac, "../data_processed/covid_country_fractional_features.rds")
message("Saved: data_processed/covid_country_fractional_features.rds")
