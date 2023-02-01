#' tab_start UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_start_ui <- function(id){
  ns <- NS(id)
  tagList(
    headerPanel(
      title = "Swiss Charge"
    ),
    sidebarLayout(
      sidebarPanel(
        mod_sort_param_ui(ns("operator_name"))
      ),
      mainPanel(
        # verbatimTextOutput(ns("test")),
        div( # tags$style("height = 1000px;"),
          leaflet::leafletOutput(ns("map"), height="80vh")
        )
      )
    )
  )
}

#' tab_start Server Functions
#'
#' @noRd
mod_tab_start_server <- function(id, data_in){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    data_out <- mod_sort_param_server("operator_name",
                          data_in = data_in,
                          sel_col = "operator_name")

    output$map <- leaflet::renderLeaflet({

      data_out() %>%
        leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        # leaflet::addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
        leaflet::addCircles(
          ~lon,
          ~lat,
          label = ~operator_name,
          fillOpacity = 0.2,
          color = ~as.character(color),
          opacity = 0,
          radius = 1500
        )
    })


  })
}

## To be copied in the UI
# mod_tab_start_ui("tab_start")

## To be copied in the server
# mod_tab_start_server("tab_start")
