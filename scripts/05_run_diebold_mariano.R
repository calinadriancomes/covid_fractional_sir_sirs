# scripts/05_run_diebold_mariano.R
#
# Runs the cross-country Diebold-Mariano benchmark and saves the outputs under
# <project root>/outputs/diebold_mariano/. Run it from the scripts/ folder (like
# scripts 01-04) or from the project root, e.g.
#   cd scripts && Rscript 05_run_diebold_mariano.R
# The root can also be forced with the environment variable COVID_PROJECT_DIR.

project_root <- Sys.getenv("COVID_PROJECT_DIR", unset = "")
if (!nzchar(project_root)) {
  project_root <- if (file.exists("global.R")) {
    getwd()
  } else if (file.exists(file.path("..", "global.R"))) {
    normalizePath("..", winslash = "/")
  } else {
    stop("Run this script from the project root or from its scripts/ folder, ",
         "or set the environment variable COVID_PROJECT_DIR.")
  }
}
source(file.path(project_root, "global.R"))

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
