# R/ml_rf.R

build_rf_dataset <- function(df_country, target_col = "target_new_confirmed_tplus7") {
  feature_cols <- c(
    "new_confirmed",
    "new_deaths",
    "new_recovered",
    "I",
    "S",
    "R_compartment",
    "incidence_rate",
    "mortality_rate",
    "recovery_rate_daily",
    "beta_naive",
    "gamma_naive",
    "lag_new_confirmed_1",
    "lag_new_confirmed_3",
    "lag_new_confirmed_7",
    "lag_I_1",
    "lag_I_7",
    "ma_new_confirmed_3",
    "ma_new_confirmed_7",
    "ma_new_confirmed_14",
    "growth_new_confirmed"
  )

  keep <- c("date", feature_cols, target_col)
  keep <- keep[keep %in% names(df_country)]

  dat <- df_country[, keep, drop = FALSE]
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]

  list(
    data = dat,
    features = setdiff(names(dat), c("date", target_col)),
    target = target_col
  )
}

split_train_test_time <- function(dat, train_ratio = 0.8) {
  n <- nrow(dat)
  n_train <- max(10, floor(n * train_ratio))

  list(
    train = dat[seq_len(n_train), , drop = FALSE],
    test  = dat[(n_train + 1):n, , drop = FALSE]
  )
}

fit_rf_model <- function(df_country,
                         target_col = "target_new_confirmed_tplus7",
                         train_ratio = 0.8,
                         ntree = 500,
                         seed = 123) {
  if (!requireNamespace("ranger", quietly = TRUE)) {
    stop("Package 'ranger' is required. Please install.packages('ranger').")
  }

  ds <- build_rf_dataset(df_country, target_col = target_col)
  spl <- split_train_test_time(ds$data, train_ratio = train_ratio)

  train <- spl$train
  test <- spl$test
  feats <- ds$features

  formula_rf <- stats::as.formula(
    paste(ds$target, "~", paste(feats, collapse = " + "))
  )

  set.seed(seed)
  mod <- ranger::ranger(
    formula = formula_rf,
    data = train,
    num.trees = ntree,
    importance = "impurity",
    seed = seed
  )

  pred_train <- stats::predict(mod, data = train)$predictions
  pred_test  <- stats::predict(mod, data = test)$predictions

  metrics <- data.frame(
    set = c("train", "test"),
    RMSE = c(rmse(train[[ds$target]], pred_train), rmse(test[[ds$target]], pred_test)),
    MAE  = c(mae(train[[ds$target]], pred_train), mae(test[[ds$target]], pred_test)),
    MAPE = c(mape(train[[ds$target]], pred_train), mape(test[[ds$target]], pred_test))
  )

  predictions_test <- data.frame(
    date = test$date,
    actual = test[[ds$target]],
    predicted = pred_test
  )

  varimp <- data.frame(
    feature = names(mod$variable.importance),
    importance = as.numeric(mod$variable.importance),
    row.names = NULL
  )
  varimp <- varimp[order(-varimp$importance), ]

  list(
    model = mod,
    metrics = metrics,
    predictions_test = predictions_test,
    variable_importance = varimp,
    features = feats,
    target = ds$target
  )
}