# modules/mod_rf_fractional.R

mod_rf_fractional_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    h4("Performance metrics"),
    tableOutput(ns("rf_frac_metrics")),
    br(),
    plotOutput(ns("plot_rf_frac_pred"), height = 360),
    br(),
    h4("Top variable importance"),
    tableOutput(ns("rf_frac_varimp"))
  )
}

mod_rf_fractional_server <- function(id, rf_frac_results) {
  moduleServer(id, function(input, output, session) {

    output$rf_frac_metrics <- renderTable({
      res <- rf_frac_results()
      req(!is.null(res))
      res$metrics
    }, digits = 4)

    output$plot_rf_frac_pred <- renderPlot({
      res <- rf_frac_results()
      req(!is.null(res), nrow(res$predictions_test) > 0)

      ggplot(res$predictions_test, aes(x = date)) +
        geom_line(aes(y = actual, color = "Actual"), linewidth = 0.7) +
        geom_line(aes(y = predicted, color = "Predicted"), linewidth = 0.7) +
        labs(
          title = "Fractional Random Forest prediction",
          x = "Date",
          y = "Target",
          color = ""
        ) +
        theme_minimal(base_size = 12)
    })

    output$rf_frac_varimp <- renderTable({
      res <- rf_frac_results()
      req(!is.null(res))
      head(res$variable_importance, 10)
    }, digits = 4)
  })
}