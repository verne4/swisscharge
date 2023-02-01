#' tab_rank UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_rank_ui <- function(id){
  ns <- NS(id)
  tagList(
    headerPanel(
      title = "Rankings data"
    ),
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("num_spot_rank")),
        mod_sort_param_ui(ns("kanton")),
        mod_sort_param_ui(ns("operator_name"))
      ),
      mainPanel(
        div(
          mod_rank_table_ui(ns("rank_table_stat_loc")),
          mod_rank_table_ui(ns("rank_table_operator")),
          mod_rank_table_ui(ns("rank_table_city"))
        )
      )
    )
  )
}

#' tab_rank Server Functions
#'
#' @noRd
mod_tab_rank_server <- function(id, data_in){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$num_spot_rank <- renderUI({
      numericInput(session$ns("rank_spots"),
                  label = stringr::str_glue("Ranking spots"),
                  value = 5,
                  min = 1,
                  max = 10)
    })


    data_sel_kanton <- mod_sort_param_server("kanton",
                                             data_in = data_in,
                                             sel_col = "kanton",
                                             rem_val = "None Listed")

    data_out <- mod_sort_param_server("operator_name",
                                      data_in = data_sel_kanton,
                                      sel_col = "operator_name")


    rank_spots_in <- reactive({input$rank_spots})

    #### Stations at a location ####
    top_stations <- reactive({
      data_out() %>%
        dplyr::count(operator_name,
                     wappen_url,
                     kanton,
                     city,
                     name = "stations",
                     sort = TRUE) %>%
        dplyr::mutate(rank = dplyr::row_number(),
                      stations_bar = stations,
                      wappen_url = stringr::str_replace_na(wappen_url,
                                   stringr::str_glue("https://upload.wikimedi\\
                                                    a.org/wikipedia/commons/t\\
                                                    humb/9/95/Coat_of_arms_o\\
                                                    f_Switzerland.svg/1082px-C\\
                                                    oat_of_arms_of_Switzerlan\\
                                                    d.svg.png"))
                      ) %>%
        dplyr::select(rank, dplyr::everything())
    })

    mod_rank_table_server("rank_table_stat_loc",
                          data_in = top_stations,
                          rank_spots = rank_spots_in,
                          header_txt = renderText({
                            stringr::str_glue("**Top {rank_spots_in()}** - L\\
                                              ocations")
                            }),
                          sub_head_txt = renderText({
                            stringr::str_glue("Locations by stations per ope\\
                                              rator")
                          }),
                          color_bar = "#44ddf9"
                          )

    # "evpass" = "#ed556f",
    # "eCarUp" = "#ffac57",
    # "Swisscharge" = "#f7fb2f",
    # "Move" = "#71ec2d",
    # "PLUG’N ROLL" = "#62ffaa",
    # "Other" = "#44ddf9"

    #### Operator stations ####
    top_operator <- reactive({
      data_out() %>%
        dplyr::count(operator_name,
                     # kanton,
                     # city,
                     name = "stations",
                     sort = TRUE) %>%
        dplyr::mutate(rank = dplyr::row_number(),
                      stations_bar = stations) %>%
        dplyr::select(rank, dplyr::everything())
    })

    mod_rank_table_server("rank_table_operator",
                          data_in = top_operator,
                          rank_spots = rank_spots_in,
                          header_txt = renderText({
                            stringr::str_glue("**Top {rank_spots_in()}** - Op\\
                                              erators")
                          }),
                          sub_head_txt = renderText({
                            stringr::str_glue("Operator by number of stations")
                          }),
                          color_bar = "#71ec2d"
                          )

    #### city stations ####
    top_city <- reactive({
      data_out() %>%
        dplyr::count(wappen_url,
                     kanton,
                     city,
                     name = "stations",
                     sort = TRUE) %>%
        dplyr::mutate(rank = dplyr::row_number(),
                      stations_bar = stations,
                      wappen_url = stringr::str_replace_na(wappen_url,
                                   stringr::str_glue("https://upload.wikimedi\\
                                                    a.org/wikipedia/commons/t\\
                                                    humb/9/95/Coat_of_arms_o\\
                                                    f_Switzerland.svg/1082px-C\\
                                                    oat_of_arms_of_Switzerlan\\
                                                    d.svg.png"))) %>%
        dplyr::select(rank, dplyr::everything())
    })

    mod_rank_table_server("rank_table_city",
                          data_in = top_city,
                          rank_spots = rank_spots_in,
                          header_txt = renderText({
                            stringr::str_glue("**Top {rank_spots_in()}** - C\\
                                              ities")
                          }),
                          sub_head_txt = renderText({
                            stringr::str_glue("Cities by number of stations")
                          }),
                          color_bar = "#ed556f"
    )
  })
}

## To be copied in the UI
# mod_tab_rank_ui("tab_rank")

## To be copied in the server
# mod_tab_rank_server("tab_rank")
