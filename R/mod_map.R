#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      leaflet::leafletOutput(ns("map"))
    ),
    hr()
  )
}

#' map Server Functions
#'
#' @noRd
mod_map_server <- function(id, data_in){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$map <- leaflet::renderLeaflet({

      # icons_list <- leaflet::icons(iconUrl =
      # )

      data_in() %>%
        dplyr::mutate(
          marker_label = stringr::str_glue(
            "{operator_name} | \\
            {city} | \\
            {kanton}"
          )
        ) %>%
        leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addMarkers(
          ~lon,
          ~lat,
          label = ~marker_label
        )
    })

  })
}

## To be copied in the UI
# mod_map_ui("map_1")

## To be copied in the server
# mod_map_server("map_1")
