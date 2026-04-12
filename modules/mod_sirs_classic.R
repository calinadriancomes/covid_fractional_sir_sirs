# modules/mod_sirs_classic.R

mod_sirs_classic_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    verbatimTextOutput(ns("sirs_parameters")),
    plotOutput(ns("plot_sirs_forecast"), height = 320),
    br(),
    plotOutput(ns("plot_sirs_incidence"), height = 300)
  )
}

mod_sirs_classic_server <- function(id, sirs_results) {
  moduleServer(id, function(input, output, session) {
    
    output$sirs_parameters <- renderPrint({
      res <- sirs_results()
      req(!is.null(res))
      
      cat("Estimated classical SIRS parameters\n")
      cat("----------------------------------\n")
      cat("beta  =", round(unique(res$beta)[1], 6), "\n")
      cat("gamma =", round(unique(res$gamma)[1], 6), "\n")
      cat("omega =", round(unique(res$omega)[1], 6), "\n")
    })
    
    output$plot_sirs_forecast <- renderPlot({
      res <- sirs_results()
      req(!is.null(res), nrow(res) > 0)
      
      ggplot(res, aes(x = date)) +
        geom_line(aes(y = S_sirs, color = "S"), linewidth = 0.8) +
        geom_line(aes(y = I_sirs, color = "I"), linewidth = 0.8) +
        geom_line(aes(y = R_sirs, color = "R"), linewidth = 0.8) +
        labs(
          title = "Classical SIRS compartments",
          x = "Date",
          y = "Population compartments",
          color = ""
        ) +
        theme_minimal(base_size = 12)
    })
    
    output$plot_sirs_incidence <- renderPlot({
      res <- sirs_results()
      req(!is.null(res), nrow(res) > 0)
      
      ggplot(res, aes(x = date, y = new_infections_sirs)) +
        geom_line(linewidth = 0.9) +
        labs(
          title = "Classical SIRS simulated incidence",
          x = "Date",
          y = "New infections"
        ) +
        theme_minimal(base_size = 12)
    })
  })
}