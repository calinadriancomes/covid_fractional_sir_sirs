# ---------------------------------------------------------------------------
# Reproduces, from the project root (auto-detected, or COVID_PROJECT_DIR), the data-processing steps
# (scripts/01..04) and the cross-country Diebold-Mariano benchmark used in the
# manuscript. Outputs are written to <root>/outputs/diebold_mariano and the
# manuscript figures to <root>/manuscript/figures.
#
# Run (from the project root):  Rscript --vanilla manuscript/reproduce/run_pipeline_for_manuscript.R
# ---------------------------------------------------------------------------
t_start <- Sys.time()
PROJECT <- Sys.getenv("COVID_PROJECT_DIR", unset = "")
if (!nzchar(PROJECT)) {
  # folder of this script (when source()d or run with Rscript), two levels below the root
  this_file <- NULL
  for (fr in rev(sys.frames())) if (is.character(fr$ofile) && nzchar(fr$ofile)) { this_file <- fr$ofile; break }
  if (is.null(this_file)) {
    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(fa) == 1) this_file <- sub("^--file=", "", fa)
  }
  PROJECT <- if (!is.null(this_file)) {
    normalizePath(file.path(dirname(this_file), "..", ".."), winslash = "/", mustWork = TRUE)
  } else {
    normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  }
}
stopifnot(file.exists(file.path(PROJECT, "global.R")))
OUT     <- file.path(PROJECT, "outputs", "diebold_mariano")
FIG_OUT <- file.path(OUT, "figures")
FIG_MS  <- file.path(PROJECT, "manuscript", "figures")
for (d in c(OUT, FIG_OUT, FIG_MS)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

log_msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " ", ...)

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2)
})

# ---- Steps 01-04 exactly as in scripts/ (they use paths relative to scripts/) ----
setwd(file.path(PROJECT, "scripts"))
log_msg("Step 01: aggregate JHU province rows to country level")
source("01_build_country_daily.R")
cd <- readRDS("../data_processed/covid_country_daily.rds")
log_msg("  country_daily: rows=", nrow(cd), " countries=", length(unique(cd$country)),
        " dates=", length(unique(cd$date)), " range=", paste(range(cd$date), collapse = " .. "))
stopifnot(length(unique(cd$date)) > 1000)

log_msg("Step 02: SIR proxies + standard ML features")
source("02_build_model_input.R")
log_msg("Step 03: fractional (Gruenwald-Letnikov) features")
source("03_build_fractional_features.R")
log_msg("Step 04: Romania ARIMA/ARFIMA comparison")
source("04_run_extended_models.R")

# ---- Step 05: cross-country Diebold-Mariano benchmark ----
setwd(PROJECT)
for (f in c("utils.R", "ml_rf.R", "ml_rf_fractional.R", "ml_xgb.R", "ml_xgb_fractional.R",
            "ml_arima.R", "ml_compare_extended.R", "diebold_mariano.R")) {
  source(file.path("R", f))
}
fmt_num <- function(x, digits = 2) formatC(x, format = "f", digits = digits, big.mark = ",")

covid_data      <- readRDS("data_processed/covid_country_model_input.rds")
covid_frac_data <- readRDS("data_processed/covid_country_fractional_features.rds")
covid_data$date <- as.Date(covid_data$date)
covid_frac_data$date <- as.Date(covid_frac_data$date)
log_msg("  model input: rows=", nrow(covid_data), " range=", paste(range(covid_data$date), collapse = " .. "),
        " fractional features=", length(grep("^frac", names(covid_frac_data))))

# progress wrapper around the per-country suite (same function, just logged)
.orig_suite <- run_country_dm_suite
.n_countries <- length(intersect(unique(covid_data$country), unique(covid_frac_data$country)))
.counter <- 0L
run_country_dm_suite <- function(...) {
  .counter <<- .counter + 1L
  a <- list(...)
  log_msg("  [", .counter, "/", .n_countries, "] ", a$country_name)
  .orig_suite(...)
}

log_msg("Step 05: Diebold-Mariano analysis (target t+7, train 0.80, min_obs 80, seed 123)")
res_dm <- run_cross_country_dm_analysis(
  covid_data = covid_data,
  covid_frac_data = covid_frac_data,
  target_col = "target_new_confirmed_tplus7",
  train_ratio = 0.80,
  min_obs = 80,
  seed = 123
)

# ---- save outputs (same layout as global.R::save_dm_outputs) ----
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
prefix <- "diebold_mariano"
save_pair <- function(obj, name, writer) {
  writer(obj, file.path(OUT, paste0(prefix, "_", name, "_", stamp, if (identical(writer, saveRDS)) ".rds" else ".csv")))
  writer(obj, file.path(OUT, paste0(prefix, "_", name, "_latest", if (identical(writer, saveRDS)) ".rds" else ".csv")))
}
wcsv <- function(obj, path) utils::write.csv(obj, path, row.names = FALSE)
saveRDS(res_dm, file.path(OUT, paste0(prefix, "_", stamp, ".rds")))
saveRDS(res_dm, file.path(OUT, paste0(prefix, "_latest.rds")))
save_pair(res_dm$by_country, "by_country", wcsv)
save_pair(res_dm$summary, "summary", wcsv)
save_pair(res_dm$fractional_dominance, "fractional_dominance", wcsv)
save_pair(if (nrow(res_dm$errors) > 0) res_dm$errors else data.frame(country = character(), error = character()), "errors", wcsv)
invisible(save_dm_pngs(res_dm, FIG_OUT, prefix = prefix))
print(res_dm$summary)
print(res_dm$fractional_dominance)

# ---- manuscript figures and ARIMA/ARFIMA fit details ----
source(file.path(PROJECT, "manuscript", "reproduce", "make_figures.R"))
source(file.path(PROJECT, "manuscript", "reproduce", "arima_arfima_fit_details.R"))
setwd(PROJECT)

writeLines(capture.output(sessionInfo()), file.path(OUT, "sessionInfo_latest.txt"))
log_msg("DONE in ", round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1), " min")
