# modules/mod_feature_lab.R

mod_feature_lab_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    plotOutput(ns("plot_fractional_features"), height = 360),
    br(),
    tableOutput(ns("table_fractional_head"))
  )
}

mod_feature_lab_server <- function(id, country_frac_data) {
  moduleServer(id, function(input, output, session) {

    output$plot_fractional_features <- renderPlot({
      dfc <- country_frac_data()
      req(nrow(dfc) > 0)

      frac_cols <- grep("^frac(diff|mem)_", names(dfc), value = TRUE)
      req(length(frac_cols) >= 2)

      pdat <- dfc[, c("date", frac_cols[1:2]), drop = FALSE]
      names(pdat)[2:3] <- c("feature_1", "feature_2")

      ggplot(pdat, aes(x = date)) +
        geom_line(aes(y = feature_1, color = "Fractional feature 1"), linewidth = 0.7) +
        geom_line(aes(y = feature_2, color = "Fractional feature 2"), linewidth = 0.7) +
        labs(
          title = "Illustration of fractional features",
          x = "Date",
          y = "Value",
          color = ""
        ) +
        theme_minimal(base_size = 12)
    })

    output$table_fractional_head <- renderTable({
      dfc <- country_frac_data()
      frac_cols <- grep("^frac(diff|mem)_", names(dfc), value = TRUE)
      keep <- c("date", "new_confirmed", frac_cols)
      keep <- keep[keep %in% names(dfc)]
      head(dfc[, keep, drop = FALSE], 10)
    }, digits = 4)
  })
}