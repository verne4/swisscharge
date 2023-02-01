#' sort_param UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sort_param_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("sel_param"))
  )
}

#' sort_param Server Functions
#'
#' @noRd
mod_sort_param_server <- function(id, data_in, sel_col, sel_val = NULL, rem_val = "none"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    sel_opt <- reactive({
      data_in() %>%
        dplyr::distinct(.data[[sel_col]]) %>%
        dplyr::filter(
          stringr::str_detect(string = .data[[sel_col]],
                              pattern = rem_val,
                              negate = TRUE)
        ) %>%
        dplyr::arrange(.data[[sel_col]]) %>%
        dplyr::pull(.data[[sel_col]])
    })

    output$sel_param <- renderUI({
      selectInput(session$ns("sel_item"),
                  label = stringr::str_glue("Select {\\
                                            stringr::str_to_title(\\
                                            stringr::str_replace_all(\\
                                            sel_col, '_', ' '))}"),
                  choices = sel_opt(),
                  selected = sel_val,
                  multiple = TRUE)
    })

    input_val <- reactive(input$sel_item)

    sel_data <- reactive({
      data_in() %>%
        # dplyr::filter(!is.na(.data[[sel_col]])) %>%
        dplyr::filter(is.null(input_val()) | .data[[sel_col]] %in% input_val())
    })

    return(sel_data)
  })
}

## To be copied in the UI
# mod_sort_param_ui("sort_param_1")

## To be copied in the server
# mod_sort_param_server("sort_param_1")
