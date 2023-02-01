#' table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      DT::DTOutput(ns("table"))
    ),
    hr()
  )
}

#' table Server Functions
#'
#' @noRd
mod_table_server <- function(id, data_in){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$table <- DT::renderDT({
      # may need to use data() when it becomes reactive
      data_in() %>%
        dplyr::arrange(city) %>%
        dplyr::select(
          kanton,
          city,
          postal_code,
          operator_name,
          stations,
          pop_plz,
          accessibility
        ) %>%
        DT::datatable(rownames = FALSE,
                      options = list(
                        pageLength = 5),
                      colnames = c('Kanton' = 'kanton',
                                   'City' = 'city',
                                   'PLZ' = 'postal_code',
                                   'Operator' = 'operator_name',
                                   'Stations' = 'stations',
                                   'Accessibility' = 'accessibility'))
    })
  })
}

## To be copied in the UI
# mod_table_ui("table_1")

## To be copied in the server
# mod_table_server("table_1")
