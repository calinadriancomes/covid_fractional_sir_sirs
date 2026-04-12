# modules/mod_sir_fractional.R

mod_sir_fractional_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    verbatimTextOutput(ns("sir_frac_parameters")),
    plotOutput(ns("plot_sir_frac_forecast"), height = 360)
  )
}

mod_sir_fractional_server <- function(id, sir_frac_results) {
  moduleServer(id, function(input, output, session) {

    output$sir_frac_parameters <- renderPrint({
      res <- sir_frac_results()
      req(!is.null(res))

      cat("Estimated fractional SIR proxy parameters\n")
      cat("-----------------------------------------\n")
      cat("beta   =", round(unique(res$beta)[1], 6), "\n")
      cat("gamma  =", round(unique(res$gamma)[1], 6), "\n")
      cat("alpha  =", round(unique(res$alpha)[1], 6), "\n")
      cat("lambda =", round(unique(res$lambda)[1], 6), "\n")
    })

    output$plot_sir_frac_forecast <- renderPlot({
      res <- sir_frac_results()
      req(!is.null(res), nrow(res) > 0)

      ggplot(res, aes(x = date)) +
        geom_line(aes(y = S_frac, color = "S_frac"), linewidth = 0.7) +
        geom_line(aes(y = I_frac, color = "I_frac"), linewidth = 0.7) +
        geom_line(aes(y = R_frac, color = "R_frac"), linewidth = 0.7) +
        labs(
          title = "Fractional SIR forecast",
          x = "Date",
          y = "Population compartments",
          color = ""
        ) +
        theme_minimal(base_size = 12)
    })
  })
}