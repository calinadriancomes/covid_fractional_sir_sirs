# R/ml_xgb.R

build_xgb_dataset <- function(df_country, target_col = "target_new_confirmed_tplus7") {
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

fit_xgb_model <- function(df_country,
                          target_col = "target_new_confirmed_tplus7",
                          train_ratio = 0.8,
                          nrounds = 300,
                          max_depth = 6,
                          eta = 0.05,
                          subsample = 0.8,
                          colsample_bytree = 0.8,
                          seed = 123) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("Package 'xgboost' is required. Please install.packages('xgboost').")
  }

  ds <- build_xgb_dataset(df_country, target_col = target_col)
  spl <- split_train_test_time(ds$data, train_ratio = train_ratio)

  train <- spl$train
  test <- spl$test
  feats <- ds$features

  x_train <- as.matrix(train[, feats, drop = FALSE])
  y_train <- train[[ds$target]]

  x_test <- as.matrix(test[, feats, drop = FALSE])
  y_test <- test[[ds$target]]

  dtrain <- xgboost::xgb.DMatrix(data = x_train, label = y_train)
  dtest  <- xgboost::xgb.DMatrix(data = x_test, label = y_test)

  set.seed(seed)
  mod <- xgboost::xgb.train(
    params = list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      max_depth = max_depth,
      eta = eta,
      subsample = subsample,
      colsample_bytree = colsample_bytree
    ),
    data = dtrain,
    nrounds = nrounds,
    verbose = 0
  )

  pred_train <- stats::predict(mod, newdata = x_train)
  pred_test  <- stats::predict(mod, newdata = x_test)

  metrics <- data.frame(
    set = c("train", "test"),
    RMSE = c(rmse(y_train, pred_train), rmse(y_test, pred_test)),
    MAE  = c(mae(y_train, pred_train), mae(y_test, pred_test)),
    MAPE = c(mape(y_train, pred_train), mape(y_test, pred_test))
  )

  predictions_test <- data.frame(
    date = test$date,
    actual = y_test,
    predicted = pred_test
  )

  varimp <- xgboost::xgb.importance(feature_names = feats, model = mod)

  list(
    model = mod,
    metrics = metrics,
    predictions_test = predictions_test,
    variable_importance = varimp,
    features = feats,
    target = ds$target
  )
}