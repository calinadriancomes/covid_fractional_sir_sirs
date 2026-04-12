# modules/mod_diebold_mariano.R

mod_diebold_mariano_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    h4("Cross-country Diebold-Mariano comparison"),
    p("The Diebold-Mariano test evaluates whether the predictive loss of the classical model differs significantly from that of the fractional alternative on the country-specific test segment."),

    h5("Family-level summary"),
    tableOutput(ns("dm_summary")),
    br(),

    h5("Synthetic indicator: Fractional Dominance Score"),
    tableOutput(ns("fds_table")),
    br(),
    plotOutput(ns("fds_plot"), height = 320),
    br(),

    h5("Family-level distribution of significant forecast dominance"),
    plotOutput(ns("family_count_plot"), height = 360),
    br(),

    h5("Interactive scrollable heatmap across countries and model families"),
    p("Use the search box to filter countries. Hover over each coloured cell to see the full numeric Diebold–Mariano profile for that country-family comparison."),
    DTOutput(ns("dm_heatmap_dt")),
    br(),

    h5("Article-ready family maps"),
    p("The faceted map below shows a separate world map for each model family. These figures are exported automatically as PNG files when the Diebold–Mariano analysis is run."),
    plotOutput(ns("dm_family_maps"), height = 980),
    br(),

    fluidRow(
      column(
        width = 8,
        h5("Global map of significant forecast dominance")
      ),
      column(
        width = 4,
        div(
          style = "text-align:right; padding-top:6px;",
          tags$img(src = "loader.gif", height = "34px", title = "Animated indicator for the interactive map panel")
        )
      )
    ),
    uiOutput(ns("map_family_ui")),
    plotOutput(ns("dm_world_map"), height = 560, click = ns("dm_world_map_click")),
    br(),

    h5("Selected country statistical cloud"),
    uiOutput(ns("country_stats_cloud")),
    br(),

    h5(textOutput(ns("selected_country_title"), inline = TRUE)),
    tableOutput(ns("dm_country")),
    br(),

    h5("Top country-family cases where the fractional model is better"),
    tableOutput(ns("fractional_top")),
    br(),

    h5("Interpretation"),
    verbatimTextOutput(ns("dm_text"))
  )
}

mod_diebold_mariano_server <- function(id, dm_results, selected_country) {
  moduleServer(id, function(input, output, session) {

    dm_summary_tbl <- reactive({
      res <- dm_results()
      validate(
        need(!is.null(res), "Diebold-Mariano analysis has not been run yet. Use the sidebar button to launch it."),
        need(!is.null(res$summary) && nrow(res$summary) > 0, "No family-level Diebold-Mariano results are available for the current settings.")
      )
      res$summary
    })

    fds_tbl <- reactive({
      res <- dm_results()
      validate(
        need(!is.null(res), "Diebold-Mariano analysis has not been run yet."),
        need(!is.null(res$fractional_dominance) && nrow(res$fractional_dominance) > 0, "No fractional dominance scores are available for the current settings.")
      )
      res$fractional_dominance
    })

    dm_country_tbl <- reactive({
      res <- dm_results()
      validate(
        need(!is.null(res), "Diebold-Mariano analysis has not been run yet."),
        need(!is.null(res$by_country) && nrow(res$by_country) > 0, "No country-level Diebold-Mariano results are available.")
      )
      out <- res$by_country
      out <- out[out$country == selected_country(), , drop = FALSE]
      validate(need(nrow(out) > 0, "No Diebold-Mariano results are available for the selected country."))
      out
    })

    family_count_tbl <- reactive({
      res <- dm_results()
      validate(
        need(!is.null(res), "Diebold-Mariano analysis has not been run yet."),
        need(!is.null(res$by_country) && nrow(res$by_country) > 0, "No country-level Diebold-Mariano results are available.")
      )
      out <- compute_dm_status_counts(res$by_country)
      validate(need(nrow(out) > 0, "No family-level distribution is available."))
      out
    })

    heatmap_tbl <- reactive({
      res <- dm_results()
      validate(
        need(!is.null(res), "Diebold-Mariano analysis has not been run yet."),
        need(!is.null(res$by_country) && nrow(res$by_country) > 0, "No country-level Diebold-Mariano results are available.")
      )
      out <- compute_dm_heatmap_data(res$by_country)
      validate(need(nrow(out) > 0, "No heatmap data are available."))
      out
    })

    fractional_top_tbl <- reactive({
      res <- dm_results()
      validate(
        need(!is.null(res), "Diebold-Mariano analysis has not been run yet."),
        need(!is.null(res$by_country) && nrow(res$by_country) > 0, "No country-level Diebold-Mariano results are available.")
      )
      df <- compute_top_fractional_countries(res$by_country, top_n = 20, significant_only = TRUE)
      validate(need(nrow(df) > 0, "No country-family comparisons currently favour the fractional model at p < 0.05."))
      df
    })

    country_stats_tbl <- reactive({
      res <- dm_results()
      validate(
        need(!is.null(res), "Diebold-Mariano analysis has not been run yet."),
        need(!is.null(res$by_country) && nrow(res$by_country) > 0, "No country-level results are available.")
      )
      out <- compute_dm_country_stats(res$by_country, selected_country())
      validate(need(nrow(out) > 0, "No statistical cloud data are available for the selected country."))
      out
    })

    output$map_family_ui <- renderUI({
      fams <- dm_summary_tbl()$family
      selectInput(
        session$ns("map_family"),
        "Model family shown on the map",
        choices = fams,
        selected = fams[1]
      )
    })

    map_data_tbl <- reactive({
      res <- dm_results()
      req(res)
      validate(need(!is.null(res$by_country) && nrow(res$by_country) > 0, "No country-level results are available for the map."))
      fam <- input$map_family
      validate(need(!is.null(fam) && nzchar(fam), "Choose a model family for the global map."))
      compute_dm_map_data(res$by_country, family = fam)
    })

    map_markers_tbl <- reactive({
      res <- dm_results()
      req(res)
      fam <- input$map_family
      validate(need(!is.null(fam) && nzchar(fam), "Choose a model family for the global map."))
      out <- compute_dm_map_markers(res$by_country, family = fam)
      validate(need(nrow(out) > 0, "No centroid marker data are available for the selected family."))
      out
    })

    output$selected_country_title <- renderText({
      paste("Selected country -", selected_country())
    })

    output$dm_summary <- renderTable({
      fmt_df(dm_summary_tbl())
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    output$fds_table <- renderTable({
      fmt_df(fds_tbl())
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    output$dm_country <- renderTable({
      fmt_df(dm_country_tbl())
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    output$fractional_top <- renderTable({
      fmt_df(fractional_top_tbl())
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    output$fds_plot <- renderPlot({
      dfp <- fds_tbl()
      ggplot(dfp, aes(x = family, y = fractional_dominance_score)) +
        geom_col() +
        geom_hline(yintercept = 0.5, linetype = "dashed") +
        labs(
          title = "Fractional Dominance Score by model family",
          x = "Model family",
          y = "Fractional dominance score"
        ) +
        ylim(0, 1) +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 20, hjust = 1))
    })

    output$family_count_plot <- renderPlot({
      dfp <- family_count_tbl()
      ggplot(dfp, aes(x = family, y = countries, fill = category)) +
        geom_col(position = "stack") +
        scale_fill_manual(
          values = c(
            "fractional better" = "#2ca25f",
            "classical better" = "#de2d26",
            "non-significant" = "#bdbdbd"
          ),
          name = "DM outcome"
        ) +
        labs(
          title = "Country counts by family and Diebold-Mariano outcome",
          x = "Model family",
          y = "Number of countries"
        ) +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "bottom")
    })

    output$dm_heatmap_dt <- renderDT({
      dfp <- heatmap_tbl()
      wide <- build_dm_heatmap_hover_table(dfp)
      family_cols <- setdiff(names(wide), "country")

      dt <- DT::datatable(
        wide,
        rownames = FALSE,
        escape = FALSE,
        filter = "top",
        options = list(
          pageLength = 25,
          scrollY = "520px",
          scrollX = TRUE,
          deferRender = TRUE,
          scroller = TRUE,
          autoWidth = TRUE,
          dom = "tip"
        ),
        class = "compact stripe hover"
      )

      for (col in family_cols) {
        dt <- DT::formatStyle(
          dt,
          columns = col,
          backgroundColor = DT::styleEqual(
            c("fractional better", "classical better", "non-significant"),
            c("#b9f6ca", "#ffcdd2", "#eeeeee")
          ),
          color = DT::styleEqual(
            c("fractional better", "classical better", "non-significant"),
            c("#0b5d1e", "#8b0000", "#555555")
          ),
          fontWeight = "bold"
        )
      }
      dt
    })

    output$dm_family_maps <- renderPlot({
      validate(need(requireNamespace("maps", quietly = TRUE), "Package 'maps' is required for the family maps. Please install.packages('maps')."))
      res <- dm_results()
      validate(need(!is.null(res) && !is.null(res$by_country) && nrow(res$by_country) > 0, "No country-level results are available for the family maps."))
      plot_dm_family_maps(res$by_country)
    })

    output$dm_world_map <- renderPlot({
      validate(need(requireNamespace("maps", quietly = TRUE), "Package 'maps' is required for the global DM map. Please install.packages('maps')."))
      world <- ggplot2::map_data("world")
      map_df <- map_data_tbl()
      marker_df <- map_markers_tbl()
      validate(need(nrow(map_df) > 0, "No map data are available for the selected model family."))

      world2 <- dplyr::left_join(world, map_df, by = "region")
      world2$map_status[is.na(world2$map_status)] <- "non-significant"
      marker_df$is_selected <- marker_df$country == selected_country()
      marker_df$popup_label <- paste0(
        marker_df$country, "\n",
        marker_df$family, "\n",
        "p = ", formatC(marker_df$dm_p_value, format = "f", digits = 3), "\n",
        "better = ", marker_df$better_model
      )
      map_title <- paste("Global Diebold-Mariano dominance map -", input$map_family)

      ggplot() +
        geom_polygon(
          data = world2,
          aes(long, lat, group = group, fill = map_status),
          color = "white",
          linewidth = 0.15
        ) +
        geom_point(
          data = marker_df,
          aes(long, lat, color = map_status),
          size = ifelse(marker_df$is_selected, 4.8, 2.1),
          alpha = 0.90
        ) +
        geom_point(
          data = marker_df[marker_df$is_selected, , drop = FALSE],
          aes(long, lat),
          shape = 21,
          fill = "gold",
          color = "black",
          stroke = 0.9,
          size = 6
        ) +
        coord_quickmap() +
        scale_fill_manual(
          values = c(
            "fractional better" = "#2ca25f",
            "classical better" = "#de2d26",
            "non-significant" = "#d9d9d9"
          ),
          breaks = c("fractional better", "classical better", "non-significant"),
          drop = FALSE,
          name = "Country shading"
        ) +
        scale_color_manual(
          values = c(
            "fractional better" = "#238b45",
            "classical better" = "#cb181d",
            "non-significant" = "#636363"
          ),
          guide = "none"
        ) +
        labs(
          title = map_title,
          subtitle = paste("Selected country:", selected_country(), "| markers summarize country-level DM outcomes")
        ) +
        theme_void(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold"),
          legend.position = "bottom"
        )
    })

    output$country_stats_cloud <- renderUI({
      df <- country_stats_tbl()
      rows <- lapply(seq_len(nrow(df)), function(i) {
        stat_col <- if (df$comparison_status[i] == "fractional better") "#2ca25f" else if (df$comparison_status[i] == "classical better") "#de2d26" else "#9e9e9e"
        tags$div(
          style = paste0(
            "display:inline-block; vertical-align:top; margin:8px; padding:14px 16px; min-width:260px; background:#ffffff; border-radius:18px; ",
            "border:2px solid ", stat_col, "; box-shadow:0 2px 10px rgba(0,0,0,0.08);"
          ),
          tags$div(style = "font-weight:700; color:#1f3c88; margin-bottom:6px;", df$family[i]),
          tags$div(tags$strong("Outcome: "), tags$span(style = paste0("color:", stat_col, "; font-weight:700;"), df$comparison_status[i])),
          tags$div(tags$strong("DM statistic: "), fmt_num(df$dm_statistic[i], 2)),
          tags$div(tags$strong("p-value: "), fmt_num(df$dm_p_value[i], 4)),
          tags$div(tags$strong("Mean loss diff: "), fmt_num(df$mean_loss_diff[i], 2)),
          tags$div(tags$strong("Variance estimator: "), df$dm_varestimator[i])
        )
      })
      tags$div(style = "padding:4px 0;", rows)
    })

    output$dm_text <- renderPrint({
      df_country <- dm_country_tbl()
      df_sum <- dm_summary_tbl()
      df_fds <- fds_tbl()

      cat("Diebold-Mariano interpretation for", selected_country(), "\n")
      cat("------------------------------------------------\n\n")

      for (i in seq_len(nrow(df_country))) {
        cat("Family:", df_country$family[i], "\n")
        cat("  DM statistic:", fmt_num(df_country$dm_statistic[i]), "\n")
        cat("  p-value:", fmt_num(df_country$dm_p_value[i], 4), "\n")
        cat("  Mean loss difference (standard - fractional):", fmt_num(df_country$mean_loss_diff[i]), "\n")
        cat("  Lower average loss:", df_country$better_model[i], "\n\n")
      }

      cat("Cross-country synthesis\n")
      cat("---------------------\n")
      for (i in seq_len(nrow(df_sum))) {
        cat(
          df_sum$family[i], ": ",
          df_sum$significant_dm_5pct[i], " significant country-level tests out of ",
          df_sum$countries_tested[i], "; dominant side by average loss = ",
          df_sum$dominant_model[i], "; FDS = ",
          fmt_num(df_fds$fractional_dominance_score[df_fds$family == df_sum$family[i]], 3),
          ".\n",
          sep = ""
        )
      }
    })
  })
}
