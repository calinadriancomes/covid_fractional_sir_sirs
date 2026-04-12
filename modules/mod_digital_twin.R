# modules/mod_digital_twin.R

mod_digital_twin_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    fluidRow(
      column(
        width = 3,
        h4("Current twin state"),
        tableOutput(ns("twin_state_tbl"))
      ),
      column(
        width = 3,
        h4("Classical SIR"),
        tableOutput(ns("sir_classic_tbl"))
      ),
      column(
        width = 3,
        h4("Fractional SIR"),
        tableOutput(ns("sir_fractional_tbl"))
      ),
      column(
        width = 3,
        h4("Scenario controls"),
        numericInput(ns("beta_scn"), "Scenario beta", value = 0.0025, step = 0.0005),
        selectInput(ns("omega_scn"), "Scenario omega", choices = c("0.001", "0.005", "0.01"), selected = "0.001"),
        actionButton(ns("run_scenario"), "Run scenario", class = "btn-primary")
      )
    ),
    br(),
    fluidRow(
      column(
        width = 6,
        h4("Digital twin forecast comparison"),
        plotOutput(ns("plot_twin_compare"), height = 380)
      ),
      column(
        width = 6,
        h4("Scenario comparison"),
        plotOutput(ns("plot_scenario_compare"), height = 380)
      )
    ),
    br(),
    fluidRow(
      column(
        width = 6,
        h4("Machine learning summary"),
        tableOutput(ns("ml_summary_tbl"))
      ),
      column(
        width = 6,
        h4("Twin interpretation"),
        verbatimTextOutput(ns("twin_text"))
      )
    ),
    br(),
    fluidRow(
      column(
        width = 12,
        h4("Forecast values"),
        tableOutput(ns("forecast_tbl"))
      )
    )
  )
}

mod_digital_twin_server <- function(
    id,
    country_data,
    sir_results,
    sir_frac_results,
    rf_results,
    xgb_results,
    rf_frac_results,
    xgb_frac_results
) {
  moduleServer(id, function(input, output, session) {
    
    twin_state_data <- reactive({
      dfc <- country_data()
      req(nrow(dfc) > 0)
      
      last_row <- tail(dfc, 1)
      
      data.frame(
        Date = as.character(last_row$date),
        Country = as.character(last_row$country),
        Susceptible = round(last_row$S, 2),
        Infected = round(last_row$I, 2),
        Removed = round(last_row$R_compartment, 2),
        New_confirmed = round(last_row$new_confirmed, 2),
        New_deaths = round(last_row$new_deaths, 2),
        New_recovered = round(last_row$new_recovered, 2),
        stringsAsFactors = FALSE
      )
    })
    
    output$twin_state_tbl <- renderTable({
      twin_state_data()
    })
    
    output$sir_classic_tbl <- renderTable({
      res <- sir_results()
      req(!is.null(res), nrow(res) > 0)
      
      data.frame(
        Parameter = c("beta", "gamma"),
        Value = c(round(unique(res$beta)[1], 6), round(unique(res$gamma)[1], 6))
      )
    })
    
    output$sir_fractional_tbl <- renderTable({
      res <- sir_frac_results()
      req(!is.null(res), nrow(res) > 0)
      
      data.frame(
        Parameter = c("beta", "gamma", "alpha", "lambda"),
        Value = c(
          round(unique(res$beta)[1], 6),
          round(unique(res$gamma)[1], 6),
          round(unique(res$alpha)[1], 4),
          round(unique(res$lambda)[1], 4)
        )
      )
    })
    
    output$ml_summary_tbl <- renderTable({
      rf_res <- rf_results()
      xgb_res <- xgb_results()
      rf_frac_res <- rf_frac_results()
      xgb_frac_res <- xgb_frac_results()
      
      req(!is.null(rf_res), !is.null(xgb_res), !is.null(rf_frac_res), !is.null(xgb_frac_res))
      
      data.frame(
        Model = c(
          "Random Forest",
          "Fractional Random Forest",
          "XGBoost",
          "Fractional XGBoost"
        ),
        RMSE_test = round(c(
          rf_res$metrics$RMSE[rf_res$metrics$set == "test"],
          rf_frac_res$metrics$RMSE[rf_frac_res$metrics$set == "test"],
          xgb_res$metrics$RMSE[xgb_res$metrics$set == "test"],
          xgb_frac_res$metrics$RMSE[xgb_frac_res$metrics$set == "test"]
        ), 2),
        MAE_test = round(c(
          rf_res$metrics$MAE[rf_res$metrics$set == "test"],
          rf_frac_res$metrics$MAE[rf_frac_res$metrics$set == "test"],
          xgb_res$metrics$MAE[xgb_res$metrics$set == "test"],
          xgb_frac_res$metrics$MAE[xgb_frac_res$metrics$set == "test"]
        ), 2),
        stringsAsFactors = FALSE
      )
    })
    
    output$forecast_tbl <- renderTable({
      sir_c <- sir_results()
      sir_f <- sir_frac_results()
      
      req(!is.null(sir_c), !is.null(sir_f), nrow(sir_c) > 0, nrow(sir_f) > 0)
      
      n <- min(nrow(sir_c), nrow(sir_f))
      
      data.frame(
        Date = as.character(sir_c$date[seq_len(n)]),
        Classical_new_infections = round(sir_c$new_infections_sim[seq_len(n)], 2),
        Fractional_new_infections = round(sir_f$new_infections_frac[seq_len(n)], 2),
        Classical_I = round(sir_c$I_sim[seq_len(n)], 2),
        Fractional_I = round(sir_f$I_frac[seq_len(n)], 2),
        stringsAsFactors = FALSE
      )
    })
    
    output$plot_twin_compare <- renderPlot({
      dfc <- country_data()
      sir_c <- sir_results()
      sir_f <- sir_frac_results()
      
      req(nrow(dfc) > 20, !is.null(sir_c), !is.null(sir_f))
      
      obs <- tail(dfc, 21)
      obs_plot <- data.frame(
        date = obs$date,
        value = obs$new_confirmed,
        series = "Observed new confirmed"
      )
      
      c_plot <- data.frame(
        date = sir_c$date,
        value = sir_c$new_infections_sim,
        series = "Classical SIR forecast"
      )
      
      f_plot <- data.frame(
        date = sir_f$date,
        value = sir_f$new_infections_frac,
        series = "Fractional SIR forecast"
      )
      
      plot_df <- dplyr::bind_rows(obs_plot, c_plot, f_plot)
      plot_df <- plot_df[is.finite(plot_df$value), , drop = FALSE]
      
      ggplot(plot_df, aes(x = date, y = value, color = series)) +
        geom_line(linewidth = 0.9) +
        labs(
          title = "Observed vs classical vs fractional forecast",
          x = "Date",
          y = "Cases / forecasted infections",
          color = ""
        ) +
        theme_minimal(base_size = 12)
    })
    
    scenario_results <- eventReactive(input$run_scenario, {
      dfc <- country_data()
      req(nrow(dfc) > 0)
      
      S0 <- tail(dfc$S, 1)
      I0 <- tail(dfc$I, 1)
      R0 <- tail(dfc$R_compartment, 1)
      N  <- S0 + I0 + R0
      
      beta_scn <- input$beta_scn
      omega_scn <- as.numeric(input$omega_scn)
      gamma_scn <- median(dfc$gamma_naive[is.finite(dfc$gamma_naive)], na.rm = TRUE)
      if (!is.finite(gamma_scn)) gamma_scn <- 0.0005
      
      sim <- simulate_sirs_discrete(
        S0 = S0,
        I0 = I0,
        R0 = R0,
        N = N,
        beta = beta_scn,
        gamma = gamma_scn,
        omega = omega_scn,
        n_steps = 90
      )
      
      sim$date <- seq(from = max(dfc$date) + 1, by = "day", length.out = 90)
      sim$beta <- beta_scn
      sim$omega <- omega_scn
      
      sim
    })
    
    output$plot_scenario_compare <- renderPlot({
      scn <- scenario_results()
      req(!is.null(scn), nrow(scn) > 0)
      
      ggplot(scn, aes(x = date)) +
        geom_line(aes(y = new_infections_sirs), linewidth = 0.9) +
        labs(
          title = paste0(
            "Scenario SIRS incidence (beta = ",
            round(unique(scn$beta)[1], 4),
            ", omega = ",
            round(unique(scn$omega)[1], 4),
            ")"
          ),
          x = "Date",
          y = "New infections"
        ) +
        theme_minimal(base_size = 12)
    })
    
    output$twin_text <- renderPrint({
      rf_res <- rf_results()
      xgb_res <- xgb_results()
      rf_frac_res <- rf_frac_results()
      xgb_frac_res <- xgb_frac_results()
      sir_f <- sir_frac_results()
      
      req(!is.null(rf_res), !is.null(xgb_res), !is.null(rf_frac_res), !is.null(xgb_frac_res), !is.null(sir_f))
      
      rf_rmse_std <- rf_res$metrics$RMSE[rf_res$metrics$set == "test"]
      rf_rmse_frac <- rf_frac_res$metrics$RMSE[rf_frac_res$metrics$set == "test"]
      xgb_rmse_std <- xgb_res$metrics$RMSE[xgb_res$metrics$set == "test"]
      xgb_rmse_frac <- xgb_frac_res$metrics$RMSE[xgb_frac_res$metrics$set == "test"]
      
      rf_gain <- 100 * (rf_rmse_std - rf_rmse_frac) / rf_rmse_std
      xgb_gain <- 100 * (xgb_rmse_std - xgb_rmse_frac) / xgb_rmse_std
      
      alpha_val <- unique(sir_f$alpha)[1]
      lambda_val <- unique(sir_f$lambda)[1]
      
      cat("This digital twin represents the selected country as a dynamically updated epidemic system.\n\n")
      cat("The classical layer tracks the current SIR state and projects future infections under short-memory assumptions.\n")
      cat("The fractional layer adds epidemic memory through alpha =", round(alpha_val, 3),
          "and lambda =", round(lambda_val, 3), ".\n\n")
      cat("Relative to the standard machine-learning baseline, the fractional feature space changes predictive accuracy as follows:\n")
      cat("- Random Forest RMSE improvement:", round(rf_gain, 2), "%\n")
      cat("- XGBoost RMSE improvement:", round(xgb_gain, 2), "%\n\n")
      cat("Scenario analysis allows explicit control of beta and omega to assess how transmission intensity and immunity loss modify epidemic recurrence.\n")
    })
  })
}