mod_validation_design_v9_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    actionButton(ns('run_panel'), 'Run Validation Design', class = 'btn-primary'),
    br(), br(),
    tabsetPanel(
      tabPanel('Train/test split', plotOutput(ns('split_plot'), height = 320), br(), tableOutput(ns('split_tbl'))),
      tabPanel('Target availability', plotOutput(ns('target_plot'), height = 320))
    )
  )
}
mod_validation_design_v9_server <- function(id, country_data, target_horizon, train_ratio) {
  moduleServer(id, function(input, output, session) {
    dat <- eventReactive(input$run_panel, {
      df <- country_data()
      n <- nrow(df)
      cut <- max(1, floor(n * train_ratio()))
      df$segment <- ifelse(seq_len(n) <= cut, 'Train', 'Test')
      list(df = df, split = data.frame(segment = c('Train','Test'), n = c(sum(df$segment=='Train'), sum(df$segment=='Test'))))
    })
    output$split_tbl <- renderTable({ dat()$split }, striped = TRUE, bordered = TRUE)
    output$split_plot <- renderPlot({
      df <- dat()$df
      ggplot(df, aes(date, new_confirmed, color = segment)) + geom_line() + theme_minimal(base_size = 12) + labs(title = 'Train/test segmentation on new_confirmed', x = 'Date', y = 'New confirmed', color = 'Segment')
    })
    output$target_plot <- renderPlot({
      df <- dat()$df
      tgt <- target_horizon()
      validate(need(tgt %in% names(df), 'Selected target horizon is not available.'))
      ggplot(df, aes(date, .data[[tgt]])) + geom_line(color = '#2C7FB8') + theme_minimal(base_size = 12) + labs(title = paste('Target availability -', tgt), x = 'Date', y = tgt)
    })
  })
}
