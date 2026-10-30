# Records, for every reporting unit, the ARIMA order selected by auto.arima, the ARFIMA
# memory parameter d and whether the ARFIMA fallback (training mean) was used.
suppressPackageStartupMessages(library(dplyr))
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
setwd(PROJECT)
source("R/utils.R"); source("R/ml_arima.R")
d <- readRDS("data_processed/covid_country_model_input.rds"); d$date <- as.Date(d$date)
out <- list()
for (ct in sort(unique(d$country))) {
  x <- d[d$country == ct, ]; x <- x[order(x$date), ]
  res <- tryCatch(compare_arima_family(x, "new_confirmed", 0.8), error = function(e) e)
  if (inherits(res, "error")) { out[[ct]] <- data.frame(country = ct, error = conditionMessage(res)); next }
  ord <- tryCatch(forecast::arimaorder(res$arima$model), error = function(e) c(NA, NA, NA))
  out[[ct]] <- data.frame(
    country = ct,
    arima_p = ord[1], arima_d = ord[2], arima_q = ord[3],
    arfima_d = res$arfima$metrics$d_fractional,
    arfima_note = res$arfima$fit_note,
    arfima_mean_fallback = grepl("mean fallback", res$arfima$fit_note, fixed = TRUE),
    rmse_arima = res$arima$metrics$RMSE, rmse_arfima = res$arfima$metrics$RMSE,
    stringsAsFactors = FALSE)
  message(ct)
}
tbl <- dplyr::bind_rows(out)
write.csv(tbl, "outputs/diebold_mariano/arima_arfima_fit_details_latest.csv", row.names = FALSE)
cat("fallback count:", sum(tbl$arfima_mean_fallback, na.rm = TRUE), "\n")
print(summary(tbl$arfima_d)); print(table(paste0("(", tbl$arima_p, ",", tbl$arima_d, ",", tbl$arima_q, ")")))
