# modules/mod_sirs_fractional.R

mod_sirs_fractional_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    verbatimTextOutput(ns("sirs_frac_parameters")),
    plotOutput(ns("plot_sirs_frac_forecast"), height = 320),
    br(),
    plotOutput(ns("plot_sirs_frac_incidence"), height = 300)
  )
}

mod_sirs_fractional_server <- function(id, sirs_frac_results) {
  moduleServer(id, function(input, output, session) {
    
    output$sirs_frac_parameters <- renderPrint({
      res <- sirs_frac_results()
      req(!is.null(res))
      
      cat("Estimated fractional SIRS proxy parameters\n")
      cat("------------------------------------------\n")
      cat("beta   =", round(unique(res$beta)[1], 6), "\n")
      cat("gamma  =", round(unique(res$gamma)[1], 6), "\n")
      cat("omega  =", round(unique(res$omega)[1], 6), "\n")
      cat("alpha  =", round(unique(res$alpha)[1], 4), "\n")
      cat("lambda =", round(unique(res$lambda)[1], 4), "\n")
    })
    
    output$plot_sirs_frac_forecast <- renderPlot({
      res <- sirs_frac_results()
      req(!is.null(res), nrow(res) > 0)
      
      ggplot(res, aes(x = date)) +
        geom_line(aes(y = S_sirs_frac, color = "S_frac"), linewidth = 0.8) +
        geom_line(aes(y = I_sirs_frac, color = "I_frac"), linewidth = 0.8) +
        geom_line(aes(y = R_sirs_frac, color = "R_frac"), linewidth = 0.8) +
        labs(
          title = "Fractional SIRS compartments",
          x = "Date",
          y = "Population compartments",
          color = ""
        ) +
        theme_minimal(base_size = 12)
    })
    
    output$plot_sirs_frac_incidence <- renderPlot({
      res <- sirs_frac_results()
      req(!is.null(res), nrow(res) > 0)
      
      ggplot(res, aes(x = date)) +
        geom_line(aes(y = new_infections_sirs_frac, color = "New infections")) +
        geom_line(aes(y = memory_term, color = "Memory term"), linetype = "dashed") +
        labs(
          title = "Fractional SIRS simulated incidence and memory",
          x = "Date",
          y = "Value",
          color = ""
        ) +
        theme_minimal(base_size = 12)
    })
  })
}