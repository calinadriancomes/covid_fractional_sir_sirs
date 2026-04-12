# global.R

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(scales)
library(htmltools)

PROJECT_DIR <- normalizePath(getwd(), winslash = '/', mustWork = FALSE)

pfile <- function(...) {
  file.path(PROJECT_DIR, ...)
}

required_pkgs <- c(
  'shiny','ggplot2','dplyr','DT','tidyr','ranger','xgboost',
  'forecast','fracdiff','rugarch','maps'
)
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    'The following required packages are missing:\n',
    paste(missing_pkgs, collapse = '\n'),
    '\n\nPlease install them before running the app.'
  )
}

required_files <- c(
  pfile('R', 'utils.R'),
  pfile('R', 'sir_classic.R'),
  pfile('R', 'sir_fractional.R'),
  pfile('R', 'ml_rf.R'),
  pfile('R', 'ml_rf_fractional.R'),
  pfile('R', 'ml_xgb.R'),
  pfile('R', 'ml_xgb_fractional.R'),
  pfile('R', 'ml_arima.R'),
  pfile('R', 'ml_compare_extended.R'),
  pfile('R', 'ml_garch.R'),
  pfile('R', 'diebold_mariano.R'),
  pfile('R', 'sirs_classic.R'),
  pfile('R', 'sirs_fractional.R'),
  pfile('data_processed', 'covid_country_model_input.rds'),
  pfile('data_processed', 'covid_country_fractional_features.rds')
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    'The following required files are missing:\n',
    paste(missing_files, collapse = '\n'),
    '\n\nPlease check your project structure.'
  )
}

source(pfile('R', 'utils.R'))
source(pfile('R', 'sir_classic.R'))
source(pfile('R', 'sir_fractional.R'))
source(pfile('R', 'ml_rf.R'))
source(pfile('R', 'ml_rf_fractional.R'))
source(pfile('R', 'ml_xgb.R'))
source(pfile('R', 'ml_xgb_fractional.R'))
source(pfile('R', 'ml_arima.R'))
source(pfile('R', 'ml_compare_extended.R'))
source(pfile('R', 'ml_garch.R'))
source(pfile('R', 'diebold_mariano.R'))
source(pfile('R', 'sirs_classic.R'))
source(pfile('R', 'sirs_fractional.R'))

covid_data <- readRDS(pfile('data_processed', 'covid_country_model_input.rds'))
covid_frac_data <- readRDS(pfile('data_processed', 'covid_country_fractional_features.rds'))

if ('date' %in% names(covid_data)) covid_data$date <- as.Date(covid_data$date) else stop("The object 'covid_data' does not contain a 'date' column.")
if ('date' %in% names(covid_frac_data)) covid_frac_data$date <- as.Date(covid_frac_data$date) else stop("The object 'covid_frac_data' does not contain a 'date' column.")

fmt_num <- function(x, digits = 3) {
  if (is.null(x)) return(x)
  if (is.numeric(x)) return(format(round(x, digits), nsmall = digits, scientific = FALSE, trim = TRUE))
  x
}

fmt_df <- function(df, digits = 3) {
  if (is.null(df) || nrow(df) == 0) return(df)
  out <- df
  for (j in seq_along(out)) if (is.numeric(out[[j]])) out[[j]] <- round(out[[j]], digits)
  out
}

safe_read_rds <- function(path) if (file.exists(path)) readRDS(path) else NULL
safe_read_csv <- function(path) if (file.exists(path)) utils::read.csv(path, stringsAsFactors = FALSE) else NULL

if (dir.exists(file.path(PROJECT_DIR, 'outputs'))) addResourcePath('proj_outputs', file.path(PROJECT_DIR, 'outputs'))
options(shiny.maxRequestSize = 100 * 1024^2)
