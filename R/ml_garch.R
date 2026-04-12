# R/ml_garch.R

build_volatility_series <- function(df_country,
                                    source_col = "new_confirmed",
                                    transform = c("logdiff", "growth")) {
  transform <- match.arg(transform)
  
  x <- as.numeric(df_country[[source_col]])
  dates <- as.Date(df_country$date)
  
  if (length(x) < 5) {
    stop("Series is too short for volatility modelling.")
  }
  
  if (transform == "logdiff") {
    y <- c(NA_real_, diff(log1p(pmax(x, 0))))
  } else {
    # More stable epidemic growth transformation
    x_smooth <- as.numeric(stats::filter(x, rep(1 / 7, 7), sides = 1))
    prev <- c(NA_real_, x_smooth[-length(x_smooth)])
    y <- (x_smooth - prev) / pmax(prev, 10)
    
    # Winsorization for numerical stability
    q_low <- stats::quantile(y, probs = 0.01, na.rm = TRUE)
    q_high <- stats::quantile(y, probs = 0.99, na.rm = TRUE)
    y <- pmin(pmax(y, q_low), q_high)
  }
  
  out <- data.frame(
    date = dates,
    series = y
  )
  
  out <- out[is.finite(out$series), , drop = FALSE]
  rownames(out) <- NULL
  
  if (nrow(out) < 50) {
    stop("Volatility series is too short after transformation.")
  }
  
  # Standardize for numerical stability
  out$series <- as.numeric(scale(out$series))
  
  out
}

split_volatility_train_test <- function(vol_df, train_ratio = 0.8) {
  n <- nrow(vol_df)
  
  if (n < 50) {
    stop("Not enough observations for train/test volatility split.")
  }
  
  n_train <- floor(n * train_ratio)
  
  if (n_train < 30) {
    n_train <- 30
  }
  
  if (n_train >= n) {
    stop("Training set consumes the full series; no test set remains.")
  }
  
  list(
    train = vol_df[seq_len(n_train), , drop = FALSE],
    test  = vol_df[seq.int(from = n_train + 1, to = n), , drop = FALSE]
  )
}

safe_infocriteria <- function(fit) {
  tryCatch(rugarch::infocriteria(fit), error = function(e) rep(NA_real_, 4))
}

safe_coef_table <- function(fit) {
  cf <- tryCatch(stats::coef(fit), error = function(e) NULL)
  
  if (is.null(cf) || length(cf) == 0) {
    return(data.frame(
      Parameter = "Unavailable",
      Estimate = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  
  data.frame(
    Parameter = names(cf),
    Estimate = as.numeric(cf),
    stringsAsFactors = FALSE
  )
}

safe_sigma_forecast <- function(fit_obj, n_ahead) {
  if (n_ahead <= 0) {
    stop("n_ahead must be strictly positive.")
  }
  
  fc <- rugarch::ugarchforecast(fit_obj, n.ahead = n_ahead)
  as.numeric(fc@forecast$sigmaFor)
}

fit_garch11_model <- function(df_country,
                              source_col = "new_confirmed",
                              train_ratio = 0.8,
                              transform = "logdiff") {
  if (!requireNamespace("rugarch", quietly = TRUE)) {
    stop("Package 'rugarch' is required. Please install.packages('rugarch').")
  }
  
  vol_df <- build_volatility_series(
    df_country = df_country,
    source_col = source_col,
    transform = transform
  )
  
  spl <- split_volatility_train_test(vol_df, train_ratio = train_ratio)
  train <- spl$train
  test  <- spl$test
  
  spec <- rugarch::ugarchspec(
    variance.model = list(
      model = "sGARCH",
      garchOrder = c(1, 1)
    ),
    mean.model = list(
      armaOrder = c(1, 0),
      include.mean = TRUE
    ),
    distribution.model = "norm"
  )
  
  fit <- tryCatch(
    rugarch::ugarchfit(
      spec = spec,
      data = train$series,
      solver = "hybrid"
    ),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    stop(paste("GARCH fit failed:", fit$message))
  }
  
  sigma_pred <- tryCatch(
    safe_sigma_forecast(fit, n_ahead = nrow(test)),
    error = function(e) rep(NA_real_, nrow(test))
  )
  
  realized_proxy <- abs(test$series)
  ic <- safe_infocriteria(fit)
  
  metrics <- data.frame(
    Model = "GARCH(1,1)",
    AIC = ic[1],
    BIC = ic[2],
    RMSE_sigma = rmse(realized_proxy, sigma_pred),
    MAE_sigma = mae(realized_proxy, sigma_pred),
    stringsAsFactors = FALSE
  )
  
  predictions_test <- data.frame(
    date = test$date,
    realized_vol_proxy = realized_proxy,
    predicted_sigma = sigma_pred
  )
  
  list(
    model = fit,
    metrics = metrics,
    predictions_test = predictions_test,
    coefficients = safe_coef_table(fit),
    series_used = vol_df,
    converged = TRUE,
    message = "GARCH converged successfully."
  )
}

fit_figarch_model <- function(df_country,
                              source_col = "new_confirmed",
                              train_ratio = 0.8,
                              transform = "logdiff") {
  if (!requireNamespace("rugarch", quietly = TRUE)) {
    stop("Package 'rugarch' is required. Please install.packages('rugarch').")
  }
  
  # Explicitly disable FIGARCH for growth transformation
  if (transform == "growth") {
    return(list(
      model = NULL,
      metrics = data.frame(
        Model = "FIGARCH(1,d,1)",
        AIC = NA_real_,
        BIC = NA_real_,
        fractional_d = NA_real_,
        RMSE_sigma = NA_real_,
        MAE_sigma = NA_real_,
        stringsAsFactors = FALSE
      ),
      predictions_test = data.frame(
        date = as.Date(character()),
        realized_vol_proxy = numeric(),
        predicted_sigma = numeric()
      ),
      coefficients = data.frame(
        Parameter = "Unavailable",
        Estimate = NA_real_,
        stringsAsFactors = FALSE
      ),
      series_used = NULL,
      converged = FALSE,
      message = "FIGARCH was not estimated for the selected growth-rate transformation because epidemic growth series may induce numerically unstable volatility structures."
    ))
  }
  
  vol_df <- build_volatility_series(
    df_country = df_country,
    source_col = source_col,
    transform = transform
  )
  
  spl <- split_volatility_train_test(vol_df, train_ratio = train_ratio)
  train <- spl$train
  test  <- spl$test
  
  spec <- rugarch::ugarchspec(
    variance.model = list(
      model = "fiGARCH",
      garchOrder = c(1, 1),
      submodel = "GARCH"
    ),
    mean.model = list(
      armaOrder = c(0, 0),
      include.mean = TRUE
    ),
    distribution.model = "norm"
  )
  
  fit <- tryCatch(
    rugarch::ugarchfit(
      spec = spec,
      data = train$series,
      solver = "hybrid"
    ),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    return(list(
      model = NULL,
      metrics = data.frame(
        Model = "FIGARCH(1,d,1)",
        AIC = NA_real_,
        BIC = NA_real_,
        fractional_d = NA_real_,
        RMSE_sigma = NA_real_,
        MAE_sigma = NA_real_,
        stringsAsFactors = FALSE
      ),
      predictions_test = data.frame(
        date = test$date,
        realized_vol_proxy = abs(test$series),
        predicted_sigma = NA_real_
      ),
      coefficients = data.frame(
        Parameter = "Unavailable",
        Estimate = NA_real_,
        stringsAsFactors = FALSE
      ),
      series_used = vol_df,
      converged = FALSE,
      message = "FIGARCH did not converge, suggesting weak long-memory volatility structure."
    ))
  }
  
  sigma_pred <- tryCatch(
    safe_sigma_forecast(fit, n_ahead = nrow(test)),
    error = function(e) rep(NA_real_, nrow(test))
  )
  
  coefs <- safe_coef_table(fit)
  ic <- safe_infocriteria(fit)
  realized_proxy <- abs(test$series)
  
  d_value <- NA_real_
  if ("delta" %in% coefs$Parameter) {
    d_value <- coefs$Estimate[coefs$Parameter == "delta"][1]
  }
  if ("d" %in% coefs$Parameter) {
    d_value <- coefs$Estimate[coefs$Parameter == "d"][1]
  }
  
  if (all(!is.finite(sigma_pred))) {
    return(list(
      model = fit,
      metrics = data.frame(
        Model = "FIGARCH(1,d,1)",
        AIC = ic[1],
        BIC = ic[2],
        fractional_d = d_value,
        RMSE_sigma = NA_real_,
        MAE_sigma = NA_real_,
        stringsAsFactors = FALSE
      ),
      predictions_test = data.frame(
        date = test$date,
        realized_vol_proxy = realized_proxy,
        predicted_sigma = NA_real_
      ),
      coefficients = coefs,
      series_used = vol_df,
      converged = FALSE,
      message = "FIGARCH did not converge, suggesting weak long-memory volatility structure."
    ))
  }
  
  metrics <- data.frame(
    Model = "FIGARCH(1,d,1)",
    AIC = ic[1],
    BIC = ic[2],
    fractional_d = d_value,
    RMSE_sigma = rmse(realized_proxy, sigma_pred),
    MAE_sigma = mae(realized_proxy, sigma_pred),
    stringsAsFactors = FALSE
  )
  
  predictions_test <- data.frame(
    date = test$date,
    realized_vol_proxy = realized_proxy,
    predicted_sigma = sigma_pred
  )
  
  list(
    model = fit,
    metrics = metrics,
    predictions_test = predictions_test,
    coefficients = coefs,
    series_used = vol_df,
    converged = TRUE,
    message = "FIGARCH converged successfully."
  )
}

compare_garch_family <- function(df_country,
                                 source_col = "new_confirmed",
                                 train_ratio = 0.8,
                                 transform = "logdiff") {
  garch_res <- fit_garch11_model(
    df_country = df_country,
    source_col = source_col,
    train_ratio = train_ratio,
    transform = transform
  )
  
  figarch_res <- fit_figarch_model(
    df_country = df_country,
    source_col = source_col,
    train_ratio = train_ratio,
    transform = transform
  )
  
  g_rmse <- garch_res$metrics$RMSE_sigma[1]
  f_rmse <- figarch_res$metrics$RMSE_sigma[1]
  g_mae  <- garch_res$metrics$MAE_sigma[1]
  f_mae  <- figarch_res$metrics$MAE_sigma[1]
  
  comparison <- dplyr::bind_rows(
    garch_res$metrics,
    figarch_res$metrics
  )
  
  gain_tbl <- data.frame(
    Family = "GARCH/FIGARCH",
    RMSE_standard = g_rmse,
    RMSE_fractional = f_rmse,
    RMSE_improvement_pct = ifelse(
      is.finite(g_rmse) & is.finite(f_rmse),
      100 * (g_rmse - f_rmse) / g_rmse,
      NA_real_
    ),
    MAE_standard = g_mae,
    MAE_fractional = f_mae,
    MAE_improvement_pct = ifelse(
      is.finite(g_mae) & is.finite(f_mae),
      100 * (g_mae - f_mae) / g_mae,
      NA_real_
    ),
    stringsAsFactors = FALSE
  )
  
  list(
    garch = garch_res,
    figarch = figarch_res,
    comparison = comparison,
    gains = gain_tbl,
    figarch_message = figarch_res$message
  )
}