mod_operator_setup_v9_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    actionButton(ns('run_panel'), 'Run Operator Setup', class = 'btn-primary'),
    br(), br(),
    tabsetPanel(
      tabPanel('Kernel comparison', plotOutput(ns('kernel_plot'), height = 360)),
      tabPanel('Fractional features', DTOutput(ns('feat_dt')))
    )
  )
}
mod_operator_setup_v9_server <- function(id, country_frac_data, alpha_reactive) {
  moduleServer(id, function(input, output, session) {
    dat <- eventReactive(input$run_panel, {
      list(kernels = build_operator_kernel_df(alpha = alpha_reactive()), feats = fractional_feature_registry(country_frac_data()))
    })
    output$kernel_plot <- renderPlot({
      df <- dat()$kernels
      ggplot(df, aes(k, value, color = operator)) + geom_line(linewidth = 1) + theme_minimal(base_size = 12) + labs(title = 'Operator kernel comparison', x = 'Lag k', y = 'Kernel weight', color = 'Operator')
    })
    output$feat_dt <- renderDT({ datatable(dat()$feats, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) })
  })
}
