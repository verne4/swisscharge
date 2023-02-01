#' tab_city UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_city_ui <- function(id){
  ns <- NS(id)
  tagList(
    # waiter::use_waiter(),
    headerPanel(
      div(
        # h1("Swiss Charge"),
        h4("Electronic car charging stations")
      )),
    sidebarLayout(
      sidebarPanel(
        mod_sort_param_ui(ns("kanton")),
        mod_sort_param_ui(ns("city")),
        mod_sort_param_ui(ns("operator_name"))
      ),
      mainPanel(
        mod_map_ui(ns("map_1")),
        mod_table_ui(ns("table_1"))
      )
    )
  )
}

#' tab_city Server Functions
#'
#' @noRd
mod_tab_city_server <- function(id, data_in){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    map_data <- reactive({
      data_in() %>%
        # dplyr::group_by(postal_code, city, street, house_num) %>%
        dplyr::group_by(lat, lon) %>%
        dplyr::mutate(stations = dplyr::n()) %>%
        dplyr::slice_head() %>%
        dplyr::ungroup()
    })

    data_sel_kanton <- mod_sort_param_server("kanton",
                                             data_in = map_data,
                                             sel_col = "kanton",
                                             sel_val = "Luzern")

    data_sel_city <- mod_sort_param_server("city",
                                           data_in = data_sel_kanton,
                                           sel_col = "city")

    data_out <- mod_sort_param_server("operator_name",
                                      data_in = data_sel_city,
                                      sel_col = "operator_name")


    mod_map_server("map_1", data_in = reactive({data_out()}))
    mod_table_server("table_1", data_in = reactive({data_out()}))

    # waiter::waiter_hide()
  })
}

## To be copied in the UI
# mod_tab_city_ui("tab_city")

## To be copied in the server
# mod_tab_city_server("tab_city")
