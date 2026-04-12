# R/ml_compare_extended.R

compute_fractional_gain <- function(std_metrics, frac_metrics, family_name) {
  data.frame(
    Family = family_name,
    RMSE_standard = std_metrics$RMSE[1],
    RMSE_fractional = frac_metrics$RMSE[1],
    RMSE_improvement_pct = 100 * (std_metrics$RMSE[1] - frac_metrics$RMSE[1]) / std_metrics$RMSE[1],
    MAE_standard = std_metrics$MAE[1],
    MAE_fractional = frac_metrics$MAE[1],
    MAE_improvement_pct = 100 * (std_metrics$MAE[1] - frac_metrics$MAE[1]) / std_metrics$MAE[1],
    MAPE_standard = std_metrics$MAPE[1],
    MAPE_fractional = frac_metrics$MAPE[1],
    MAPE_improvement_pct = 100 * (std_metrics$MAPE[1] - frac_metrics$MAPE[1]) / std_metrics$MAPE[1]
  )
}

run_extended_model_comparison <- function(df_standard,
                                          target_col = "new_confirmed",
                                          train_ratio = 0.8) {
  arima_cmp <- compare_arima_family(
    df_country = df_standard,
    target_col = target_col,
    train_ratio = train_ratio
  )
  
  gain_tbl <- dplyr::bind_rows(
    compute_fractional_gain(
      arima_cmp$arima$metrics,
      arima_cmp$arfima$metrics,
      "ARIMA/ARFIMA"
    )
  )
  
  list(
    arima = arima_cmp,
    gains = gain_tbl
  )
}