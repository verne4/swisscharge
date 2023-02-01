#' tab_kanton UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_kanton_ui <- function(id){
  ns <- NS(id)
  tagList(
    headerPanel(
      title = "Kanton Charts"
    ),
    sidebarLayout(
      sidebarPanel(
        mod_sort_param_ui(ns("kanton")),
        mod_sort_param_ui(ns("operator_name"))
      ),
      mainPanel(
        div(
          mod_kanton_bar_op_ui(ns("kanton_bar_op")),
          mod_kanton_geofacet_op_ui(ns("kanton_geofacet_op")),
          mod_kanton_gt_stations_ui(ns("kanton_gt_stations"))
        )
    )

    )
  )
}

#' tab_kanton Server Functions
#'
#' @noRd
mod_tab_kanton_server <- function(id, data_in){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    data_sel_kanton <- mod_sort_param_server("kanton",
                                             data_in = data_in,
                                             sel_col = "kanton",
                                             rem_val = "None Listed")

    data_out <- mod_sort_param_server("operator_name",
                                      data_in = data_sel_kanton,
                                      sel_col = "operator_name")

    #### Plots ####

    mod_kanton_bar_op_server("kanton_bar_op",
                             data_in = data_out
                             )

    mod_kanton_geofacet_op_server("kanton_geofacet_op",
                                  data_in = data_out
                                  )

    mod_kanton_gt_stations_server("kanton_gt_stations",
                                  data_in = reactive({data_out()})
                                  )

  })
}

## To be copied in the UI
# mod_tab_kanton_ui("tab_kanton")

## To be copied in the server
# mod_tab_kanton_server("tab_kanton")
