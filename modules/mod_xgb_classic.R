# modules/mod_xgb_classic.R

mod_xgb_classic_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    h4("Performance metrics"),
    tableOutput(ns("xgb_metrics")),
    br(),
    plotOutput(ns("plot_xgb_pred"), height = 360),
    br(),
    h4("Top variable importance"),
    tableOutput(ns("xgb_varimp"))
  )
}

mod_xgb_classic_server <- function(id, xgb_results) {
  moduleServer(id, function(input, output, session) {

    output$xgb_metrics <- renderTable({
      res <- xgb_results()
      req(!is.null(res))
      res$metrics
    }, digits = 4)

    output$plot_xgb_pred <- renderPlot({
      res <- xgb_results()
      req(!is.null(res), nrow(res$predictions_test) > 0)

      ggplot(res$predictions_test, aes(x = date)) +
        geom_line(aes(y = actual, color = "Actual"), linewidth = 0.7) +
        geom_line(aes(y = predicted, color = "Predicted"), linewidth = 0.7) +
        labs(
          title = "XGBoost prediction",
          x = "Date",
          y = "Target",
          color = ""
        ) +
        theme_minimal(base_size = 12)
    })

    output$xgb_varimp <- renderTable({
      res <- xgb_results()
      req(!is.null(res))
      vi <- res$variable_importance
      if (is.null(vi) || nrow(vi) == 0) {
        return(data.frame(Message = "No variable importance available"))
      }
      head(vi, 10)
    }, digits = 4)
  })
}