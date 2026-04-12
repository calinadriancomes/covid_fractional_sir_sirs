# modules/mod_downloads.R
mod_downloads_ui <- function(id) {
  ns <- NS(id)
  tagList(h4("Downloads module - to be implemented"))
}

mod_downloads_server <- function(id, ...) {
  moduleServer(id, function(input, output, session) {})
}