# scripts/05_run_diebold_mariano.R

source(file.path("C:/date/covid_fractional_sir_project_v4", "global.R"))

res_dm <- run_cross_country_dm_analysis(
  covid_data = covid_data,
  covid_frac_data = covid_frac_data,
  target_col = "target_new_confirmed_tplus7",
  train_ratio = 0.80,
  min_obs = 80,
  seed = 123
)

saved_paths <- save_dm_outputs(res_dm)

message("Diebold-Mariano analysis completed and saved successfully.")
message("Latest RDS: ", saved_paths$latest_rds)
message("Latest country CSV: ", saved_paths$latest_by_country_csv)
message("Latest summary CSV: ", saved_paths$latest_summary_csv)
message("Latest errors CSV: ", saved_paths$latest_errors_csv)


cat("Fractional dominance score table:
")
print(res_dm$fractional_dominance)
