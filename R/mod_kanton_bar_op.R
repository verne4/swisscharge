#' kanton_bar_op UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_kanton_bar_op_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      plotOutput(ns("kanton_col")),
      downloadLink(ns("download_bar"), "Download"),
      tags$hr()
    )
  )
}

#' kanton_bar_op Server Functions
#'
#' @noRd
mod_kanton_bar_op_server <- function(id, data_in){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    plot_bar <- reactive({
      data_in() %>%
        dplyr::count(kanton_kz, op_color, name = "stations") %>%
        ggplot2::ggplot(ggplot2::aes(y = stations,
                                     x = kanton_kz,
                                     fill = op_color)) +
        ggplot2::geom_col() +
        ggplot2::scale_fill_manual(values = data_in()$color) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = "Charging Stations per Kanton",
          subtitle = "Total number of stations colored by operator grouping",
          fill = "Top 5 Operators",
          x = "Kanton Abbreviation",
          y = "Number of Stations"
        )
    })

    output$kanton_col <- renderPlot({plot_bar()})

    output$download_bar <-  downloadHandler(
      # filename ="plot_bar.png",
      filename = function() {
        stringr::str_glue(
          "Stations per Kanton {Sys.Date()}.png"
        )
      },

      content = function(file){
        ggplot2::ggsave(filename = file,
                        plot = plot_bar(),
                        device = "png",
                        width = 1500,
                        height = 700,
                        units = "px",
                        scale = 3)
      })

  })
}

## To be copied in the UI
# mod_kanton_bar_op_ui("kanton_bar_op_1")

## To be copied in the server
# mod_kanton_bar_op_server("kanton_bar_op_1")
