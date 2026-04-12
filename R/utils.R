# R/utils.R

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

clip_nonnegative <- function(x) {
  pmax(0, x)
}

lag_vec <- function(x, k = 1) {
  if (k < 0) stop("k must be non-negative.")
  c(rep(NA, k), head(x, -k))
}

lead_vec <- function(x, k = 1) {
  if (k < 0) stop("k must be non-negative.")
  c(tail(x, -k), rep(NA, k))
}

rolling_mean_simple <- function(x, k = 7) {
  stats::filter(x, rep(1 / k, k), sides = 1)
}

growth_rate <- function(x) {
  prev <- lag_vec(x, 1)
  out <- (x - prev) / pmax(prev, 1)
  out[!is.finite(out)] <- NA_real_
  out
}

rmse <- function(actual, predicted) {
  idx <- is.finite(actual) & is.finite(predicted)
  sqrt(mean((actual[idx] - predicted[idx])^2))
}

mae <- function(actual, predicted) {
  idx <- is.finite(actual) & is.finite(predicted)
  mean(abs(actual[idx] - predicted[idx]))
}

mape <- function(actual, predicted) {
  idx <- is.finite(actual) & is.finite(predicted) & actual != 0
  mean(abs((actual[idx] - predicted[idx]) / actual[idx])) * 100
}