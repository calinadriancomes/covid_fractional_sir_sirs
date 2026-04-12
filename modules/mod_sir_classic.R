# modules/mod_sir_classic.R

mod_sir_classic_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    verbatimTextOutput(ns("sir_parameters")),
    plotOutput(ns("plot_sir_forecast"), height = 320),
    br(),
    plotOutput(ns("plot_sir_incidence"), height = 300)
  )
}

mod_sir_classic_server <- function(id, sir_results) {
  moduleServer(id, function(input, output, session) {
    
    output$sir_parameters <- renderPrint({
      res <- sir_results()
      req(!is.null(res))
      
      cat("Estimated classical SIR parameters\n")
      cat("----------------------------------\n")
      cat("beta  =", round(unique(res$beta)[1], 6), "\n")
      cat("gamma =", round(unique(res$gamma)[1], 6), "\n")
    })
    
    output$plot_sir_forecast <- renderPlot({
      res <- sir_results()
      req(!is.null(res), nrow(res) > 0)
      
      ggplot(res, aes(x = date)) +
        geom_line(aes(y = S_sim, color = "S"), linewidth = 0.7) +
        geom_line(aes(y = I_sim, color = "I"), linewidth = 0.7) +
        geom_line(aes(y = R_sim, color = "R"), linewidth = 0.7) +
        labs(
          title = "Classical SIR forecast",
          x = "Date",
          y = "Population compartments",
          color = ""
        ) +
        theme_minimal(base_size = 12)
    })
    
    output$plot_sir_incidence <- renderPlot({
      res <- sir_results()
      req(!is.null(res), nrow(res) > 0)
      
      ggplot(res, aes(x = date, y = new_infections_sim)) +
        geom_line(linewidth = 0.9) +
        labs(
          title = "Classical SIR simulated incidence",
          x = "Date",
          y = "New infections"
        ) +
        theme_minimal(base_size = 12)
    })
  })
}