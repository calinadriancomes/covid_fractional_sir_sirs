mod_country_level_v9_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    actionButton(ns('run_panel'), 'Run Country-Level panel', class = 'btn-primary'),
    br(), br(),
    tabsetPanel(
      tabPanel('Selected country profile', plotOutput(ns('country_plot'), height = 340), br(), tableOutput(ns('country_tbl'))),
      tabPanel('Ranking', plotOutput(ns('rank_plot'), height = 340), br(), tableOutput(ns('rank_tbl'))),
      tabPanel('Country vs global', plotOutput(ns('vs_global_plot'), height = 340))
    )
  )
}
mod_country_level_v9_server <- function(id, selected_country, dm_results) {
  moduleServer(id, function(input, output, session) {
    panel_data <- eventReactive(input$run_panel, {
      res <- dm_results()
      if (is.null(res) || is.null(res$by_country) || nrow(res$by_country) == 0) res <- latest_dm_snapshot()
      res$by_country
    })
    country_tbl <- reactive({
      df <- panel_data(); validate(need(!is.null(df) && nrow(df) > 0, 'No country-level DM results are available.'))
      dplyr::filter(df, .data$country == selected_country())
    })
    output$country_tbl <- renderTable({ fmt_df(country_tbl()) }, striped = TRUE, bordered = TRUE)
    output$country_plot <- renderPlot({
      df <- country_tbl(); validate(need(nrow(df) > 0, 'No DM profile is available for the selected country.'))
      ggplot(df, aes(family, dm_statistic, fill = better_model)) + geom_col() + geom_hline(yintercept = 0, linetype = 'dashed') +
        theme_minimal(base_size = 12) + coord_flip() + labs(title = paste('DM profile -', selected_country()), x = '', y = 'DM statistic', fill = 'Better model')
    })
    rank_tbl <- reactive({
      df <- panel_data(); validate(need(!is.null(df) && nrow(df) > 0, 'No country-level DM results are available.'))
      fam <- unique(df$family)[1]
      country_rank_table(df, fam, metric = 'mean_loss_diff') |> dplyr::slice_head(n = 20)
    })
    output$rank_tbl <- renderTable({ fmt_df(rank_tbl()) }, striped = TRUE, bordered = TRUE)
    output$rank_plot <- renderPlot({
      df <- rank_tbl(); validate(need(nrow(df) > 0, 'No ranking table is available.'))
      ggplot(df, aes(reorder(country, rank_metric), rank_metric, fill = better_model)) + geom_col() + coord_flip() +
        theme_minimal(base_size = 12) + labs(title = 'Top cross-country loss differentials for first family', x = '', y = 'Mean loss difference', fill = 'Better model')
    })
    output$vs_global_plot <- renderPlot({
      df <- panel_data(); validate(need(!is.null(df) && nrow(df) > 0, 'No country-level DM results are available.'))
      sel <- dplyr::filter(df, .data$country == selected_country()) |> dplyr::select(family, selected_dm = dm_statistic)
      glob <- df |> dplyr::group_by(family) |> dplyr::summarise(global_mean_dm = mean(dm_statistic, na.rm = TRUE), .groups='drop')
      plot_df <- dplyr::left_join(sel, glob, by = 'family') |> tidyr::pivot_longer(cols = c('selected_dm','global_mean_dm'), names_to = 'series', values_to = 'value')
      ggplot(plot_df, aes(family, value, fill = series)) + geom_col(position = 'dodge') + theme_minimal(base_size = 12) + coord_flip() +
        labs(title = paste(selected_country(), 'vs global mean DM'), x = '', y = 'DM statistic', fill = '')
    })
  })
}
