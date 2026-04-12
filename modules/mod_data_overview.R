# modules/mod_data_overview.R

mod_data_overview_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    h4(textOutput(ns("country_title"), inline = TRUE)),
    tableOutput(ns("country_summary")),
    br(),
    plotOutput(ns("plot_series"), height = 360),
    br(),
    DTOutput(ns("table_tail"))
  )
}

mod_data_overview_server <- function(id, country_data, country_frac_data) {
  moduleServer(id, function(input, output, session) {

    output$country_title <- renderText({
      dfc <- country_data()
      req(nrow(dfc) > 0)
      paste("Data overview for", unique(dfc$country))
    })

    output$country_summary <- renderTable({
      dfc <- country_data()
      dff <- country_frac_data()
      req(nrow(dfc) > 0)

      data.frame(
        Country = unique(dfc$country)[1],
        Observations_standard = nrow(dfc),
        Observations_fractional = nrow(dff),
        Start_date = as.character(min(dfc$date, na.rm = TRUE)),
        End_date = as.character(max(dfc$date, na.rm = TRUE)),
        Mean_new_confirmed = mean(dfc$new_confirmed, na.rm = TRUE),
        Mean_new_deaths = mean(dfc$new_deaths, na.rm = TRUE),
        Mean_new_recovered = mean(dfc$new_recovered, na.rm = TRUE),
        stringsAsFactors = FALSE
      ) |> fmt_df()
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    output$plot_series <- renderPlot({
      dfc <- country_data()
      req(nrow(dfc) > 0)

      ggplot(dfc, aes(x = date)) +
        geom_line(aes(y = new_confirmed, color = "New confirmed"), linewidth = 0.6, na.rm = TRUE) +
        geom_line(aes(y = new_deaths, color = "New deaths"), linewidth = 0.6, na.rm = TRUE) +
        geom_line(aes(y = new_recovered, color = "New recovered"), linewidth = 0.6, na.rm = TRUE) +
        labs(
          title = paste("Daily COVID-19 series -", unique(dfc$country)),
          x = "Date",
          y = "Counts",
          color = ""
        ) +
        theme_minimal(base_size = 12)
    })

    output$table_tail <- renderDT({
      dfc <- country_data()
      req(nrow(dfc) > 0)
      datatable(
        tail(dfc, 15),
        options = list(pageLength = 15, scrollX = TRUE)
      )
    })
  })
}
