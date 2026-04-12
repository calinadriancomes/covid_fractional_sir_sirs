# modules/mod_model_compare.R

mod_model_compare_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    h4("Test-set comparison"),
    tableOutput(ns("comparison_metrics")),
    br(),
    h4("Percentage improvement from the fractional layer"),
    plotOutput(ns("plot_improvement"), height = 380)
  )
}

mod_model_compare_server <- function(id, rf_results, xgb_results, rf_frac_results, xgb_frac_results) {
  moduleServer(id, function(input, output, session) {
    
    comparison_data <- reactive({
      rf_res <- rf_results()
      xgb_res <- xgb_results()
      rf_frac_res <- rf_frac_results()
      xgb_frac_res <- xgb_frac_results()
      
      req(!is.null(rf_res), !is.null(xgb_res), !is.null(rf_frac_res), !is.null(xgb_frac_res))
      
      rf_rmse_std <- rf_res$metrics$RMSE[rf_res$metrics$set == "test"]
      rf_mae_std  <- rf_res$metrics$MAE[rf_res$metrics$set == "test"]
      rf_mape_std <- rf_res$metrics$MAPE[rf_res$metrics$set == "test"]
      
      rf_rmse_frac <- rf_frac_res$metrics$RMSE[rf_frac_res$metrics$set == "test"]
      rf_mae_frac  <- rf_frac_res$metrics$MAE[rf_frac_res$metrics$set == "test"]
      rf_mape_frac <- rf_frac_res$metrics$MAPE[rf_frac_res$metrics$set == "test"]
      
      xgb_rmse_std <- xgb_res$metrics$RMSE[xgb_res$metrics$set == "test"]
      xgb_mae_std  <- xgb_res$metrics$MAE[xgb_res$metrics$set == "test"]
      xgb_mape_std <- xgb_res$metrics$MAPE[xgb_res$metrics$set == "test"]
      
      xgb_rmse_frac <- xgb_frac_res$metrics$RMSE[xgb_frac_res$metrics$set == "test"]
      xgb_mae_frac  <- xgb_frac_res$metrics$MAE[xgb_frac_res$metrics$set == "test"]
      xgb_mape_frac <- xgb_frac_res$metrics$MAPE[xgb_frac_res$metrics$set == "test"]
      
      comparison_tbl <- data.frame(
        Model = c(
          "Random Forest",
          "Fractional Random Forest",
          "XGBoost",
          "Fractional XGBoost"
        ),
        RMSE_test = c(
          rf_rmse_std,
          rf_rmse_frac,
          xgb_rmse_std,
          xgb_rmse_frac
        ),
        MAE_test = c(
          rf_mae_std,
          rf_mae_frac,
          xgb_mae_std,
          xgb_mae_frac
        ),
        MAPE_test = c(
          rf_mape_std,
          rf_mape_frac,
          xgb_mape_std,
          xgb_mape_frac
        ),
        RMSE_improvement_pct = c(
          NA,
          100 * (rf_rmse_std - rf_rmse_frac) / rf_rmse_std,
          NA,
          100 * (xgb_rmse_std - xgb_rmse_frac) / xgb_rmse_std
        ),
        MAE_improvement_pct = c(
          NA,
          100 * (rf_mae_std - rf_mae_frac) / rf_mae_std,
          NA,
          100 * (xgb_mae_std - xgb_mae_frac) / xgb_mae_std
        ),
        MAPE_improvement_pct = c(
          NA,
          100 * (rf_mape_std - rf_mape_frac) / rf_mape_std,
          NA,
          100 * (xgb_mape_std - xgb_mape_frac) / xgb_mape_std
        )
      )
      
      improvement_plot_df <- data.frame(
        Model = rep(c("Random Forest", "XGBoost"), each = 3),
        Metric = rep(c("RMSE improvement", "MAE improvement", "MAPE improvement"), times = 2),
        Improvement = c(
          100 * (rf_rmse_std - rf_rmse_frac) / rf_rmse_std,
          100 * (rf_mae_std - rf_mae_frac) / rf_mae_std,
          100 * (rf_mape_std - rf_mape_frac) / rf_mape_std,
          100 * (xgb_rmse_std - xgb_rmse_frac) / xgb_rmse_std,
          100 * (xgb_mae_std - xgb_mae_frac) / xgb_mae_std,
          100 * (xgb_mape_std - xgb_mape_frac) / xgb_mape_std
        )
      )
      
      list(
        comparison_tbl = comparison_tbl,
        improvement_plot_df = improvement_plot_df
      )
    })
    
    output$comparison_metrics <- renderTable({
      comparison_data()$comparison_tbl
    }, digits = 4)
    
    output$plot_improvement <- renderPlot({
      dfp <- comparison_data()$improvement_plot_df
      req(nrow(dfp) > 0)
      
      ggplot(dfp, aes(x = Metric, y = Improvement, fill = Model)) +
        geom_col(position = position_dodge(width = 0.8), width = 0.7) +
        geom_text(
          aes(label = round(Improvement, 2)),
          position = position_dodge(width = 0.8),
          vjust = -0.4,
          size = 4
        ) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        labs(
          title = "Percentage improvement brought by fractional features",
          x = "",
          y = "Improvement (%)",
          fill = ""
        ) +
        theme_minimal(base_size = 12)
    })
  })
}