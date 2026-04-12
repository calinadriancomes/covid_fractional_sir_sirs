mod_pairwise_dm_v9_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    actionButton(ns('run_panel'), 'Run Pairwise DM panel', class = 'btn-primary'),
    br(), br(),
    tabsetPanel(
      tabPanel('Family summary', plotOutput(ns('summary_plot'), height = 340), br(), tableOutput(ns('summary_tbl'))),
      tabPanel('DM distribution', plotOutput(ns('dist_plot'), height = 340)),
      tabPanel('Loss differential cloud', plotOutput(ns('cloud_plot'), height = 360), br(), DTOutput(ns('top_dt')))
    )
  )
}
mod_pairwise_dm_v9_server <- function(id, dm_results) {
  moduleServer(id, function(input, output, session) {
    panel_data <- eventReactive(input$run_panel, {
      res <- dm_results()
      if (is.null(res) || is.null(res$by_country) || nrow(res$by_country) == 0) res <- latest_dm_snapshot()
      res
    })
    output$summary_tbl <- renderTable({ fmt_df(panel_data()$summary) }, striped = TRUE, bordered = TRUE)
    output$summary_plot <- renderPlot({
      df <- panel_data()$summary
      validate(need(!is.null(df) && nrow(df) > 0, 'No family-level DM summary is available.'))
      ggplot(df, aes(family, fractional_dominance_score, fill = dominance_label)) + geom_col() +
        theme_minimal(base_size = 12) + coord_flip() + labs(title = 'Family-level fractional dominance score', x = '', y = 'Fractional dominance score', fill = 'Dominance')
    })
    output$dist_plot <- renderPlot({
      df <- panel_data()$by_country
      validate(need(!is.null(df) && nrow(df) > 0, 'No country-level DM results are available.'))
      ggplot(df, aes(dm_statistic, fill = family)) + geom_histogram(bins = 30, alpha = 0.7, position = 'identity') +
        facet_wrap(~family, scales = 'free_y') + theme_minimal(base_size = 12) + labs(title = 'Distribution of Diebold-Mariano statistics', x = 'DM statistic', y = 'Count', fill = 'Family')
    })
    output$cloud_plot <- renderPlot({
      df <- panel_data()$by_country
      validate(need(!is.null(df) && nrow(df) > 0, 'No country-level DM results are available.'))
      ggplot(df, aes(mean_loss_diff, -log10(pmax(dm_p_value, 1e-12)), color = better_model)) + geom_point(alpha = 0.7) +
        facet_wrap(~family, scales = 'free') + theme_minimal(base_size = 12) +
        labs(title = 'Loss differential cloud', x = 'Mean loss difference (standard - fractional)', y = '-log10(p-value)', color = 'Better model')
    })
    output$top_dt <- renderDT({
      df <- panel_data()$by_country
      validate(need(!is.null(df) && nrow(df) > 0, 'No country-level DM results are available.'))
      df <- df |> dplyr::arrange(.data$dm_p_value, dplyr::desc(abs(.data$mean_loss_diff))) |> dplyr::slice_head(n = 25)
      datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
  })
}
