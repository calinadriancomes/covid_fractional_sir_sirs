# app.R

source("global.R")

# ------------------------------------------------------------
# Source Shiny modules
# ------------------------------------------------------------
source(pfile("modules", "mod_data_overview.R"))
source(pfile("modules", "mod_feature_lab.R"))

source(pfile("modules", "mod_sir_classic.R"))
source(pfile("modules", "mod_sir_fractional.R"))

source(pfile("modules", "mod_sirs_classic.R"))
source(pfile("modules", "mod_sirs_fractional.R"))

source(pfile("modules", "mod_rf_classic.R"))
source(pfile("modules", "mod_rf_fractional.R"))

source(pfile("modules", "mod_xgb_classic.R"))
source(pfile("modules", "mod_xgb_fractional.R"))

source(pfile("modules", "mod_model_compare.R"))
source(pfile("modules", "mod_digital_twin.R"))
source(pfile("modules", "mod_volatility.R"))

# ------------------------------------------------------------
# Source extended families
# ------------------------------------------------------------
source(pfile("R", "ml_arima.R"))
source(pfile("R", "ml_compare_extended.R"))
source(pfile("R", "ml_garch.R"))

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
ui <- fluidPage(
  titlePanel("COVID-19 SIR, SIRS, Fractional Modelling, Machine Learning, and Digital Twin Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      
      selectInput(
        inputId = "country",
        label = "Select country",
        choices = sort(unique(covid_data$country)),
        selected = if ("Romania" %in% unique(covid_data$country)) "Romania" else unique(covid_data$country)[1]
      ),
      
      selectInput(
        inputId = "target_horizon",
        label = "Prediction horizon",
        choices = c(
          "t+1" = "target_new_confirmed_tplus1",
          "t+7" = "target_new_confirmed_tplus7"
        ),
        selected = "target_new_confirmed_tplus7"
      ),
      
      numericInput(
        inputId = "sir_horizon",
        label = "Forecast horizon",
        value = 90,
        min = 7,
        max = 365,
        step = 1
      ),
      
      sliderInput(
        inputId = "train_ratio",
        label = "Train ratio",
        min = 0.60,
        max = 0.90,
        value = 0.80,
        step = 0.05
      ),
      
      sliderInput(
        inputId = "alpha_frac",
        label = "Fractional alpha",
        min = 0.2,
        max = 0.8,
        value = 0.6,
        step = 0.2
      ),
      
      actionButton("run_models", "Run models", class = "btn-primary"),
      br(), br(),
      
      helpText("This interface compares classical and fractional epidemiological modelling strategies, including SIR and SIRS, together with machine learning, volatility, and digital twin perspectives.")
    ),
    
    mainPanel(
      width = 10,
      
      tabsetPanel(
        tabPanel("Data overview", mod_data_overview_ui("overview")),
        tabPanel("Feature laboratory", mod_feature_lab_ui("feature_lab")),
        
        tabPanel(
          "SIR",
          tabsetPanel(
            tabPanel("Classical SIR", mod_sir_classic_ui("sir_classic")),
            tabPanel("Fractional SIR", mod_sir_fractional_ui("sir_fractional"))
          )
        ),
        
        tabPanel(
          "SIRS",
          tabsetPanel(
            tabPanel("Classical SIRS", mod_sirs_classic_ui("sirs_classic")),
            tabPanel("Fractional SIRS", mod_sirs_fractional_ui("sirs_fractional"))
          )
        ),
        
        tabPanel(
          "Random Forest",
          tabsetPanel(
            tabPanel("Classical RF", mod_rf_classic_ui("rf_classic")),
            tabPanel("Fractional RF", mod_rf_fractional_ui("rf_fractional"))
          )
        ),
        
        tabPanel(
          "XGBoost",
          tabsetPanel(
            tabPanel("Classical XGBoost", mod_xgb_classic_ui("xgb_classic")),
            tabPanel("Fractional XGBoost", mod_xgb_fractional_ui("xgb_fractional"))
          )
        ),
        
        tabPanel("Model comparison", mod_model_compare_ui("compare")),
        tabPanel("Volatility", mod_volatility_ui("volatility")),
        tabPanel("Digital Twin", mod_digital_twin_ui("digital_twin")),
        
        tabPanel(
          "Extended Models",
          br(),
          h4("ARIMA vs ARFIMA"),
          tableOutput("ext_table"),
          br(),
          plotOutput("ext_plot", height = 360),
          br(),
          plotOutput("ext_pred_plot", height = 360),
          br(),
          h4("Interpretation"),
          verbatimTextOutput("ext_text")
        )
      )
    )
  )
)

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # Country-level data
  # ----------------------------------------------------------
  country_data <- reactive({
    req(input$country)
    covid_data %>%
      dplyr::filter(country == input$country) %>%
      dplyr::arrange(date)
  })
  
  country_frac_data <- reactive({
    req(input$country)
    covid_frac_data %>%
      dplyr::filter(country == input$country) %>%
      dplyr::arrange(date)
  })
  
  # ----------------------------------------------------------
  # Main modelling results
  # ----------------------------------------------------------
  model_results <- eventReactive(input$run_models, {
    dfc <- country_data()
    dfcf <- country_frac_data()
    
    validate(
      need(nrow(dfc) >= 30, "Not enough observations for modelling."),
      need(input$target_horizon %in% names(dfc), "Selected target horizon is not available in the standard data."),
      need(input$target_horizon %in% names(dfcf), "Selected target horizon is not available in the fractional data.")
    )
    
    # --------------------------
    # Random Forest
    # --------------------------
    rf_res <- fit_rf_model(
      df_country = dfc,
      target_col = input$target_horizon,
      train_ratio = input$train_ratio,
      ntree = 500,
      seed = 123
    )
    
    rf_frac_res <- fit_rf_fractional_model(
      df_country = dfcf,
      target_col = input$target_horizon,
      train_ratio = input$train_ratio,
      ntree = 500,
      seed = 123
    )
    
    # --------------------------
    # XGBoost
    # --------------------------
    xgb_res <- fit_xgb_model(
      df_country = dfc,
      target_col = input$target_horizon,
      train_ratio = input$train_ratio,
      nrounds = 300,
      max_depth = 6,
      eta = 0.05,
      subsample = 0.8,
      colsample_bytree = 0.8,
      seed = 123
    )
    
    xgb_frac_res <- fit_xgb_fractional_model(
      df_country = dfcf,
      target_col = input$target_horizon,
      train_ratio = input$train_ratio,
      nrounds = 300,
      max_depth = 6,
      eta = 0.05,
      subsample = 0.8,
      colsample_bytree = 0.8,
      seed = 123
    )
    
    # --------------------------
    # SIR
    # --------------------------
    sir_res <- run_sir_forecast(
      df_country = dfc,
      horizon = input$sir_horizon
    )
    
    sir_frac_res <- run_fractional_sir_forecast(
      df_country = dfc,
      horizon = input$sir_horizon,
      alpha = input$alpha_frac,
      k_max = 14
    )
    
    # --------------------------
    # SIRS
    # --------------------------
    sirs_res <- run_sirs_forecast(
      df_country = dfc,
      horizon = input$sir_horizon
    )
    
    sirs_frac_res <- run_fractional_sirs_forecast(
      df_country = dfc,
      horizon = input$sir_horizon,
      alpha = input$alpha_frac,
      k_max = 14
    )
    
    list(
      rf = rf_res,
      rf_frac = rf_frac_res,
      xgb = xgb_res,
      xgb_frac = xgb_frac_res,
      sir = sir_res,
      sir_frac = sir_frac_res,
      sirs = sirs_res,
      sirs_frac = sirs_frac_res
    )
  })
  
  # ----------------------------------------------------------
  # Reactive accessors
  # ----------------------------------------------------------
  rf_results <- reactive({ model_results()$rf })
  rf_frac_results <- reactive({ model_results()$rf_frac })
  
  xgb_results <- reactive({ model_results()$xgb })
  xgb_frac_results <- reactive({ model_results()$xgb_frac })
  
  sir_results <- reactive({ model_results()$sir })
  sir_frac_results <- reactive({ model_results()$sir_frac })
  
  sirs_results <- reactive({ model_results()$sirs })
  sirs_frac_results <- reactive({ model_results()$sirs_frac })
  
  # ----------------------------------------------------------
  # Extended ARIMA / ARFIMA
  # ----------------------------------------------------------
  extended_results <- eventReactive(input$run_models, {
    df_std <- country_data()
    
    validate(
      need(nrow(df_std) >= 60, "Not enough observations for extended modelling.")
    )
    
    run_extended_model_comparison(
      df_standard = df_std,
      target_col = "new_confirmed",
      train_ratio = input$train_ratio
    )
  })
  
  # ----------------------------------------------------------
  # Register Shiny modules
  # ----------------------------------------------------------
  mod_data_overview_server("overview", country_data = country_data)
  mod_feature_lab_server("feature_lab", country_frac_data = country_frac_data)
  
  # SIR
  mod_sir_classic_server("sir_classic", sir_results = sir_results)
  mod_sir_fractional_server("sir_fractional", sir_frac_results = sir_frac_results)
  
  # SIRS
  mod_sirs_classic_server("sirs_classic", sirs_results = sirs_results)
  mod_sirs_fractional_server("sirs_fractional", sirs_frac_results = sirs_frac_results)
  
  # Random Forest
  mod_rf_classic_server("rf_classic", rf_results = rf_results)
  mod_rf_fractional_server("rf_fractional", rf_frac_results = rf_frac_results)
  
  # XGBoost
  mod_xgb_classic_server("xgb_classic", xgb_results = xgb_results)
  mod_xgb_fractional_server("xgb_fractional", xgb_frac_results = xgb_frac_results)
  
  # Comparison
  mod_model_compare_server(
    "compare",
    rf_results = rf_results,
    xgb_results = xgb_results,
    rf_frac_results = rf_frac_results,
    xgb_frac_results = xgb_frac_results
  )
  
  # Digital Twin
  mod_digital_twin_server(
    "digital_twin",
    country_data = country_data,
    sir_results = sir_results,
    sir_frac_results = sir_frac_results,
    rf_results = rf_results,
    xgb_results = xgb_results,
    rf_frac_results = rf_frac_results,
    xgb_frac_results = xgb_frac_results
  )
  
  # Volatility
  mod_volatility_server(
    "volatility",
    country_data = country_data,
    train_ratio_reactive = reactive(input$train_ratio)
  )
  
  # ----------------------------------------------------------
  # Extended models outputs
  # ----------------------------------------------------------
  output$ext_table <- renderTable({
    res <- extended_results()
    req(!is.null(res))
    fmt_df(res$gains)
  })
  
  output$ext_plot <- renderPlot({
    res <- extended_results()
    req(!is.null(res))
    
    df <- res$gains
    
    df_long <- tidyr::pivot_longer(
      df,
      cols = c("RMSE_improvement_pct", "MAE_improvement_pct", "MAPE_improvement_pct"),
      names_to = "metric",
      values_to = "value"
    )
    
    df_long <- df_long[is.finite(df_long$value), , drop = FALSE]
    
    ggplot(df_long, aes(x = Family, y = value, fill = metric)) +
      geom_col(position = "dodge") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(
        title = "Fractional improvement in the ARIMA family",
        x = "Model family",
        y = "Improvement (%)",
        fill = "Metric"
      ) +
      theme_minimal(base_size = 12)
  })
  
  output$ext_pred_plot <- renderPlot({
    res <- extended_results()
    req(!is.null(res))
    
    std_pred <- res$arima$arima$predictions_test
    frac_pred <- res$arima$arfima$predictions_test
    
    std_pred <- std_pred[is.finite(std_pred$actual) & is.finite(std_pred$predicted), , drop = FALSE]
    frac_pred <- frac_pred[is.finite(frac_pred$actual) & is.finite(frac_pred$predicted), , drop = FALSE]
    
    n <- min(nrow(std_pred), nrow(frac_pred))
    req(n > 0)
    
    plot_df <- data.frame(
      date = c(std_pred$date[seq_len(n)], frac_pred$date[seq_len(n)], std_pred$date[seq_len(n)]),
      value = c(
        std_pred$predicted[seq_len(n)],
        frac_pred$predicted[seq_len(n)],
        std_pred$actual[seq_len(n)]
      ),
      series = c(
        rep("ARIMA", n),
        rep("ARFIMA", n),
        rep("Observed", n)
      )
    )
    
    ggplot(plot_df, aes(x = date, y = value, color = series)) +
      geom_line(linewidth = 0.8) +
      labs(
        title = "Observed vs ARIMA vs ARFIMA",
        x = "Date",
        y = "New confirmed cases",
        color = ""
      ) +
      theme_minimal(base_size = 12)
  })
  
  output$ext_text <- renderPrint({
    res <- extended_results()
    req(!is.null(res))
    
    gains <- res$gains
    
    cat("Extended model comparison\n")
    cat("-------------------------\n\n")
    
    for (i in seq_len(nrow(gains))) {
      cat("Family:", gains$Family[i], "\n")
      cat("  RMSE improvement (%): ", fmt_num(gains$RMSE_improvement_pct[i]), "\n", sep = "")
      cat("  MAE improvement (%):  ", fmt_num(gains$MAE_improvement_pct[i]), "\n", sep = "")
      cat("  MAPE improvement (%): ", fmt_num(gains$MAPE_improvement_pct[i]), "\n\n", sep = "")
    }
    
    cat("Interpretation:\n")
    cat("Positive values indicate that the fractional layer improved predictive accuracy relative to the standard ARIMA baseline.\n")
  })
}

# ------------------------------------------------------------
# Run app
# ------------------------------------------------------------
shinyApp(ui = ui, server = server)