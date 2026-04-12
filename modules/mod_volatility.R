# modules/mod_volatility.R

mod_volatility_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    fluidRow(
      column(
        width = 4,
        selectInput(
          ns("vol_transform"),
          "Volatility transformation",
          choices = c("Log-difference" = "logdiff", "Growth rate" = "growth"),
          selected = "logdiff"
        )
      ),
      column(
        width = 4,
        actionButton(ns("run_volatility"), "Run volatility models", class = "btn-primary")
      )
    ),
    br(),
    h4("GARCH vs FIGARCH comparison"),
    tableOutput(ns("vol_table")),
    br(),
    h4("Percentage improvement"),
    plotOutput(ns("vol_gain_plot"), height = 320),
    br(),
    h4("Volatility forecast comparison"),
    plotOutput(ns("vol_pred_plot"), height = 360),
    br(),
    h4("FIGARCH parameters"),
    tableOutput(ns("figarch_coef_table")),
    br(),
    verbatimTextOutput(ns("vol_text"))
  )
}

mod_volatility_server <- function(id, country_data, train_ratio_reactive) {
  moduleServer(id, function(input, output, session) {
    
    vol_results <- eventReactive(input$run_volatility, {
      dfc <- country_data()
      req(nrow(dfc) >= 80)
      
      compare_garch_family(
        df_country = dfc,
        source_col = "new_confirmed",
        train_ratio = train_ratio_reactive(),
        transform = input$vol_transform
      )
    })
    
    output$vol_table <- renderTable({
      res <- vol_results()
      req(!is.null(res))
      res$comparison
    }, digits = 4)
    
    output$vol_gain_plot <- renderPlot({
      res <- vol_results()
      req(!is.null(res))
      
      df <- data.frame(
        Metric = c("RMSE improvement", "MAE improvement"),
        Improvement = c(
          res$gains$RMSE_improvement_pct[1],
          res$gains$MAE_improvement_pct[1]
        )
      )
      
      df <- df[is.finite(df$Improvement), , drop = FALSE]
      
      if (nrow(df) == 0) {
        plot.new()
        text(
          0.5, 0.5,
          res$figarch_message,
          cex = 1.0
        )
        return()
      }
      
      ggplot(df, aes(x = Metric, y = Improvement)) +
        geom_col(width = 0.6) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_text(aes(label = round(Improvement, 2)), vjust = -0.3, size = 4) +
        labs(
          title = "Fractional gain in volatility modelling",
          x = "",
          y = "Improvement (%)"
        ) +
        theme_minimal(base_size = 12)
    })
    
    output$vol_pred_plot <- renderPlot({
      res <- vol_results()
      req(!is.null(res))
      
      g <- res$garch$predictions_test
      f <- res$figarch$predictions_test
      
      if (!res$figarch$converged || nrow(f) == 0 || all(!is.finite(f$predicted_sigma))) {
        plot.new()
        text(
          0.5, 0.5,
          res$figarch_message,
          cex = 1.0
        )
        return()
      }
      
      g <- g[is.finite(g$realized_vol_proxy) & is.finite(g$predicted_sigma), , drop = FALSE]
      f <- f[is.finite(f$realized_vol_proxy) & is.finite(f$predicted_sigma), , drop = FALSE]
      
      n <- min(nrow(g), nrow(f))
      req(n > 0)
      
      idx <- seq_len(n)
      
      plot_df <- data.frame(
        date = c(g$date[idx], f$date[idx], g$date[idx]),
        value = c(
          g$predicted_sigma[idx],
          f$predicted_sigma[idx],
          g$realized_vol_proxy[idx]
        ),
        series = c(
          rep("GARCH sigma", n),
          rep("FIGARCH sigma", n),
          rep("Realized volatility proxy", n)
        )
      )
      
      ggplot(plot_df, aes(x = date, y = value, color = series)) +
        geom_line(linewidth = 0.8) +
        labs(
          title = "Conditional volatility: GARCH vs FIGARCH",
          x = "Date",
          y = "Volatility",
          color = ""
        ) +
        theme_minimal(base_size = 12)
    })
    
    output$figarch_coef_table <- renderTable({
      res <- vol_results()
      req(!is.null(res))
      res$figarch$coefficients
    }, digits = 5)
    
    output$vol_text <- renderPrint({
      res <- vol_results()
      req(!is.null(res))
      
      rmse_gain <- res$gains$RMSE_improvement_pct[1]
      mae_gain  <- res$gains$MAE_improvement_pct[1]
      
      d_value <- NA_real_
      cf <- res$figarch$coefficients
      if ("delta" %in% cf$Parameter) {
        d_value <- cf$Estimate[cf$Parameter == "delta"][1]
      }
      if ("d" %in% cf$Parameter) {
        d_value <- cf$Estimate[cf$Parameter == "d"][1]
      }
      
      cat("Volatility interpretation\n")
      cat("-------------------------\n\n")
      cat("This module models epidemic instability rather than the level of cases.\n")
      cat("GARCH(1,1) captures conditional heteroscedasticity, whereas FIGARCH(1,d,1) introduces fractional persistence in volatility.\n\n")
      
      if (!res$figarch$converged) {
        cat(res$figarch_message, "\n")
      } else {
        cat("Estimated fractional parameter d:", round(d_value, 4), "\n")
        cat("RMSE improvement from FIGARCH:", round(rmse_gain, 2), "%\n")
        cat("MAE improvement from FIGARCH:", round(mae_gain, 2), "%\n")
      }
    })
  })
}