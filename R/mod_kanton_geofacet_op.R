#' kanton_geofacet_op UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_kanton_geofacet_op_ui <- function(id){
  ns <- NS(id)
  div(
    plotOutput(ns("geo_facet_map")),
    downloadLink(ns("download_geo"), "Download"),
    tags$hr()
  )
}

#' kanton_geofacet_op Server Functions
#'
#' @noRd
mod_kanton_geofacet_op_server <- function(id, data_in){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    plot_geo <- reactive({
      data_in() %>%
        dplyr::filter(!is.na(kanton_kz)) %>%
        ggplot2::ggplot(ggplot2::aes(y = "", fill = op_color)) +
        ggplot2::geom_bar(position = "fill") +
        geofacet::facet_geo(ggplot2::vars(kanton_kz),
                            grid = geofacet::ch_cantons_grid2) +
        ggplot2::theme_bw() +
        ggplot2::scale_fill_manual(values = data_in()$color) +
        ggplot2::scale_x_continuous(labels = scales::percent) +
        ggplot2::labs(
          title = "Percentage of stations by operator per Kanton ",
          y = "",
          x = "Kanton",
          fill = "Top 5 Operators"
        ) +
        ggplot2::theme(
          axis.title = ggplot2::element_text(NULL)
        )
    })

    output$geo_facet_map <- renderPlot({plot_geo()})

    output$download_geo <-  downloadHandler(
      filename = function() {
        stringr::str_glue(
          "Operators per Kanton {Sys.Date()}.png"
        )
        },

    content = function(file){
      ggplot2::ggsave(filename = file,
                      plot = plot_geo(),
                      device = "png",
                      width = 1500,
                      height = 700,
                      units = "px",
                      scale = 3)
    })

  })
}

## To be copied in the UI
# mod_kanton_geofacet_op_ui("kanton_geofacet_op_1")

## To be copied in the server
# mod_kanton_geofacet_op_server("kanton_geofacet_op_1")
