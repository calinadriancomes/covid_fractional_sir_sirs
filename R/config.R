# R/config.R

default_config <- list(
  countries_default = c("Romania", "Italy", "US"),
  horizon = 7,
  train_ratio = 0.8,
  population_fallback = 1e7,
  active_window_days = 14,
  ma_windows = c(3, 7, 14),
  fractional_alphas = c(0.2, 0.4, 0.6, 0.8),
  rf = list(
    ntree = 500,
    mtry = NULL,
    min_node_size = 5,
    seed = 123
  ),
  xgb = list(
    nrounds = 300,
    max_depth = 6,
    eta = 0.05,
    subsample = 0.8,
    colsample_bytree = 0.8,
    objective = "reg:squarederror",
    eval_metric = "rmse",
    seed = 123
  )
)