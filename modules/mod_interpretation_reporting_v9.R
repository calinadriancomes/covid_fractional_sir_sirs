mod_interpretation_reporting_v9_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    actionButton(ns('run_panel'), 'Run Interpretation panel', class = 'btn-primary'),
    br(), br(),
    tabsetPanel(
      tabPanel('Interpretation', br(), plotOutput(ns('score_plot'), height = 320), br(), verbatimTextOutput(ns('text'))),
      tabPanel('Outputs registry', br(), plotOutput(ns('out_plot'), height = 320), br(), DTOutput(ns('out_dt'))),
      tabPanel('Figure gallery', br(), uiOutput(ns('gallery')), br(), plotOutput(ns('size_plot'), height = 320))
    )
  )
}
mod_interpretation_reporting_v9_server <- function(id, dm_results) {
  moduleServer(id, function(input, output, session) {
    panel_data <- eventReactive(input$run_panel, {
      res <- dm_results()
      dm_summary <- NULL
      if (!is.null(res) && !is.null(res$summary) && nrow(res$summary) > 0) dm_summary <- res$summary else dm_summary <- latest_dm_snapshot()$summary
      registry <- build_output_registry()
      fig_dir <- file.path(PROJECT_DIR, 'outputs', 'diebold_mariano', 'figures')
      figs <- if (dir.exists(fig_dir)) list.files(fig_dir, pattern = '[.]png$', full.names = FALSE) else character(0)
      list(dm_summary = dm_summary, registry = registry, figures = figs)
    })
    output$score_plot <- renderPlot({
      df <- panel_data()$dm_summary
      validate(need(!is.null(df) && nrow(df) > 0, 'No summary table is available. Run Cross-Country DM first.'))
      fill_var <- if ('dominant_model' %in% names(df)) 'dominant_model' else if ('dominance_label' %in% names(df)) 'dominance_label' else NULL
      y_var <- if ('countries_tested' %in% names(df)) 'countries_tested' else if ('fractional_dominance_score' %in% names(df)) 'fractional_dominance_score' else NULL
      validate(need('family' %in% names(df), "Column 'family' is missing from DM summary."), need(!is.null(y_var), "Neither 'countries_tested' nor 'fractional_dominance_score' was found in DM summary."))
      if (!is.null(fill_var)) {
        ggplot(df, aes(x = family, y = .data[[y_var]], fill = .data[[fill_var]])) + geom_col() + coord_flip() + theme_minimal(base_size = 12) + labs(title = 'Family-level summary', x = '', y = y_var, fill = fill_var)
      } else {
        ggplot(df, aes(x = family, y = .data[[y_var]])) + geom_col(fill = '#2C7FB8') + coord_flip() + theme_minimal(base_size = 12) + labs(title = 'Family-level summary', x = '', y = y_var)
      }
    })
    output$text <- renderPrint({
      df <- panel_data()$dm_summary
      validate(need(!is.null(df) && nrow(df) > 0, 'No summary table is available. Run Cross-Country DM first.'))
      cat('Interpretation scaffold for v9\n')
      cat('------------------------------\n\n')
      for (i in seq_len(nrow(df))) {
        fam <- if ('family' %in% names(df)) df$family[i] else paste('Family', i)
        dom <- if ('dominant_model' %in% names(df)) df$dominant_model[i] else if ('dominance_label' %in% names(df)) df$dominance_label[i] else 'unknown'
        score <- if ('fractional_dominance_score' %in% names(df)) fmt_num(df$fractional_dominance_score[i], 3) else 'NA'
        cat('* ', fam, ': ', dom, ' dominates in the current cross-country summary, with fractional dominance score = ', score, '.\n', sep = '')
      }
      cat('\nMedical/epidemiological reading: lower loss means more stable forecasting of epidemic burden, which supports better timing for surveillance and resource allocation.\n')
    })
    output$out_dt <- renderDT({
      df <- panel_data()$registry
      validate(need(!is.null(df) && nrow(df) > 0, 'No exported files were detected.'))
      datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
    output$out_plot <- renderPlot({
      df <- panel_data()$registry
      validate(need(!is.null(df) && nrow(df) > 0, 'No exported files were detected.'))
      agg <- df |> dplyr::group_by(group) |> dplyr::summarise(n = dplyr::n(), .groups = 'drop')
      ggplot(agg, aes(x = group, y = n, fill = group)) + geom_col(show.legend = FALSE) + theme_minimal(base_size = 12) + labs(title = 'Exported output registry', x = '', y = 'File count')
    })
    output$gallery <- renderUI({
      figs <- head(panel_data()$figures, 12)
      if (length(figs) == 0) return(tags$p('No PNG figure exports detected yet.'))
      do.call(tagList, lapply(figs, function(f) {
        img_src <- paste0('proj_outputs/diebold_mariano/figures/', utils::URLencode(f, reserved = TRUE))
        tags$div(style = 'margin-bottom:18px;', tags$strong(f), br(), tags$img(src = img_src, style = 'max-width:100%; height:auto; border:1px solid #ddd; padding:4px; background:#fff;'))
      }))
    })
    output$size_plot <- renderPlot({
      df <- panel_data()$registry
      validate(need(!is.null(df) && nrow(df) > 0, 'No exported files were detected.'))
      top <- df |> dplyr::arrange(dplyr::desc(size_kb)) |> dplyr::slice_head(n = 12)
      ggplot(top, aes(x = reorder(file, size_kb), y = size_kb, fill = group)) + geom_col() + coord_flip() + theme_minimal(base_size = 12) + labs(title = 'Largest exported files', x = '', y = 'Size (KB)', fill = 'Group')
    })
  })
}
