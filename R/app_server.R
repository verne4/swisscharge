#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  elektro_data <- reactive(load_data())

  mod_tab_start_server("tab_start", data_in = elektro_data)
  mod_tab_kanton_server("tab_kanton", data_in = elektro_data)
  mod_tab_city_server("tab_city", data_in = elektro_data)
  mod_tab_rank_server("tab_rank", data_in = elektro_data)
  mod_tab_contact_server("tab_contact")

}
