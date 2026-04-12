library(dplyr)

source("../R/utils.R")
source("../R/ml_arima.R")
source("../R/ml_compare_extended.R")

df_standard <- readRDS("../data_processed/covid_country_model_input.rds")

country_sel <- "Romania"

df_std_country <- df_standard |>
  dplyr::filter(country == country_sel) |>
  dplyr::arrange(date)

res_all <- run_extended_model_comparison(
  df_standard = df_std_country,
  target_col = "new_confirmed",
  train_ratio = 0.8
)

print(res_all$gains)

saveRDS(res_all, "../data_processed/extended_model_comparison_romania.rds")
message("Saved: ../data_processed/extended_model_comparison_romania.rds")