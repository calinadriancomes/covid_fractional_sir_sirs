# R/ml_arima.R

fit_arima_model <- function(df_country,
                            target_col = "new_confirmed",
                            train_ratio = 0.8) {
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("Package 'forecast' is required. Please install.packages('forecast').")
  }

  y <- df_country[[target_col]]
  idx <- is.finite(y)
  y <- y[idx]
  dates <- df_country$date[idx]

  n <- length(y)
  n_train <- max(30, floor(n * train_ratio))
  n_train <- min(n_train, n - 1)

  y_train <- y[1:n_train]
  y_test  <- y[(n_train + 1):n]
  test_dates <- dates[(n_train + 1):n]

  fit <- forecast::auto.arima(y_train, seasonal = FALSE)
  fc  <- forecast::forecast(fit, h = length(y_test))
  pred <- as.numeric(fc$mean)

  metrics <- data.frame(
    Model = "ARIMA",
    RMSE = rmse(y_test, pred),
    MAE = mae(y_test, pred),
    MAPE = mape(y_test, pred),
    stringsAsFactors = FALSE
  )

  predictions_test <- data.frame(
    date = test_dates,
    actual = y_test,
    predicted = pred
  )

  list(
    model = fit,
    metrics = metrics,
    predictions_test = predictions_test
  )
}

.safe_fracdiff_fit <- function(y_train) {
  warn_msg <- character(0)

  fit_fd <- tryCatch(
    withCallingHandlers(
      fracdiff::fracdiff(y_train, nar = 1, nma = 1),
      warning = function(w) {
        warn_msg <<- c(warn_msg, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )

  failed_main <- inherits(fit_fd, "error") ||
    any(grepl("optimization failure", warn_msg, ignore.case = TRUE))

  if (failed_main) {
    fit_fd_alt <- tryCatch(
      withCallingHandlers(
        fracdiff::fracdiff(y_train, nar = 0, nma = 0),
        warning = function(w) {
          warn_msg <<- c(warn_msg, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) e
    )

    if (!inherits(fit_fd_alt, "error")) {
      return(list(
        fit = fit_fd_alt,
        note = "fracdiff(1,1) did not converge cleanly; fallback fracdiff(0,0) was used."
      ))
    }

    return(list(
      fit = NULL,
      note = "fracdiff optimization failed; fallback forecasting rule was used."
    ))
  }

  note_txt <- if (length(warn_msg) == 0) {
    "OK"
  } else {
    paste(unique(warn_msg), collapse = " | ")
  }

  list(
    fit = fit_fd,
    note = note_txt
  )
}

fit_arfima_model <- function(df_country,
                             target_col = "new_confirmed",
                             train_ratio = 0.8) {
  if (!requireNamespace("fracdiff", quietly = TRUE)) {
    stop("Package 'fracdiff' is required. Please install.packages('fracdiff').")
  }
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("Package 'forecast' is required. Please install.packages('forecast').")
  }

  y <- df_country[[target_col]]
  idx <- is.finite(y)
  y <- y[idx]
  dates <- df_country$date[idx]

  n <- length(y)
  n_train <- max(30, floor(n * train_ratio))
  n_train <- min(n_train, n - 1)

  y_train <- y[1:n_train]
  y_test  <- y[(n_train + 1):n]
  test_dates <- dates[(n_train + 1):n]

  fd_info <- .safe_fracdiff_fit(y_train)
  fit_fd <- fd_info$fit
  fit_note <- fd_info$note

  pred <- NULL
  fit_obj <- NULL
  arfima_warn <- character(0)

  arfima_attempt <- tryCatch(
    withCallingHandlers(
      {
        if ("arfima" %in% getNamespaceExports("forecast")) {
          model_tmp <- forecast::arfima(y_train)
          fc_tmp <- forecast::forecast(model_tmp, h = length(y_test))
          list(model = model_tmp, pred = as.numeric(fc_tmp$mean), ok = TRUE)
        } else {
          list(model = NULL, pred = NULL, ok = FALSE)
        }
      },
      warning = function(w) {
        arfima_warn <<- c(arfima_warn, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      list(model = NULL, pred = NULL, ok = FALSE, error = conditionMessage(e))
    }
  )

  if (length(arfima_warn) > 0) {
    fit_note <- paste(c(fit_note, unique(arfima_warn)), collapse = " | ")
  }

  if (isTRUE(arfima_attempt$ok) && !is.null(arfima_attempt$pred) && all(is.finite(arfima_attempt$pred))) {
    fit_obj <- arfima_attempt$model
    pred <- arfima_attempt$pred
  } else {
    pred <- rep(mean(y_train, na.rm = TRUE), length(y_test))
    fit_obj <- if (!is.null(fit_fd)) fit_fd else "mean_fallback"
    fit_note <- paste(
      c(fit_note, "forecast::arfima unavailable or unstable; mean fallback forecast used."),
      collapse = " | "
    )
  }

  d_value <- if (!is.null(fit_fd) && !is.null(fit_fd$d)) fit_fd$d else NA_real_

  metrics <- data.frame(
    Model = "ARFIMA",
    d_fractional = d_value,
    RMSE = rmse(y_test, pred),
    MAE = mae(y_test, pred),
    MAPE = mape(y_test, pred),
    fit_note = fit_note,
    stringsAsFactors = FALSE
  )

  predictions_test <- data.frame(
    date = test_dates,
    actual = y_test,
    predicted = pred
  )

  list(
    model = fit_obj,
    fracdiff_fit = fit_fd,
    metrics = metrics,
    predictions_test = predictions_test,
    fit_note = fit_note
  )
}

compare_arima_family <- function(df_country,
                                 target_col = "new_confirmed",
                                 train_ratio = 0.8) {
  arima_res <- fit_arima_model(df_country, target_col, train_ratio)
  arfima_res <- fit_arfima_model(df_country, target_col, train_ratio)

  comparison <- dplyr::bind_rows(
    arima_res$metrics,
    arfima_res$metrics
  )

  list(
    arima = arima_res,
    arfima = arfima_res,
    comparison = comparison
  )
}
