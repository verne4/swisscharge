#' rank_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rank_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
    gt::gt_output(ns("rank_tbl")),
    downloadLink(ns("downloadData"), label = "Download"),
    tags$hr()
    )
  )
}

#' rank_table Server Functions
#'
#' @noRd
mod_rank_table_server <- function(id,
                                  data_in,
                                  rank_spots,
                                  header_txt = "",
                                  sub_head_txt = "",
                                  color_bar = "black"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    data_slice <- reactive({
      req(rank_spots())
      data_in() %>%
        dplyr::slice_head(n = as.integer(rank_spots()))
    })

    footer_check <- reactive({
      stringr::str_detect(
        stringr::str_flatten(
          colnames(data_slice())),
        "kanton")
    })

    operator_check <- reactive({
      stringr::str_detect(
        stringr::str_flatten(
          colnames(data_slice())),
        "operator_name")
    })

    gt_tbl <- reactive({

      req(data_slice())

      data_slice() %>%
        gt::gt() %>%
        gt::tab_header(
          title = gt::md(header_txt()),
          subtitle = gt::md(sub_head_txt())
        ) %>%
        gt::opt_row_striping() %>%
        gt::fmt_integer(columns = stations,
                        sep_mark = "'") %>%
        {
          if(footer_check())
            gt::tab_footnote(data = .,
              footnote = "Charging stations missing Kanton label.",
              locations = gt::cells_body(columns = kanton,
                                         rows = kanton == "None Listed")
            )
          else
            .
        } %>%
        {
          if(footer_check())
            gt::text_transform(data = .,
              locations = gt::cells_body(c(wappen_url)),
              fn = function(x) {
                gt::web_image(
                  url = x,
                  height = "20px"
                )
              }
            ) %>%
            gt::cols_label(.data = .,
                           wappen_url = "")
          else
            .
        } %>%
        {
          if(operator_check())
        gt::cols_label(.data = .,
          operator_name = "operator name"
        )
          else
            .
          } %>%
        gtExtras::gt_plt_bar(column = stations_bar,
                             keep_column = FALSE,
                             color = color_bar) %>%
        gt::cols_label(stations_bar = "") %>%
        gtExtras::gt_theme_538()
    })

    output$rank_tbl <- gt::render_gt(
      expr = gt_tbl(),
      align = "left"
    )


    header_txt_download <- reactive({
      stringr::str_replace(header_txt(), "^*.* - ", "Top ")
    })

    # fine name code from example on the Shiny website:
    # https://shiny.rstudio.com/reference/shiny/latest/downloadbutton
    output$downloadData <- downloadHandler(
      filename = function() {
        stringr::str_glue(
          "{header_txt_download()} {Sys.Date()}.csv"
          )
      },
      content = function(file) {
        write.csv(data_slice() %>%
                    {
                      if(footer_check())
                        dplyr::select(.data = ., -wappen_url, -stations_bar)
                      else
                        .
                      },
                  file, row.names = FALSE)
      }
    )

  })
}

## To be copied in the UI
# mod_rank_table_ui("rank_table_1")

## To be copied in the server
# mod_rank_table_server("rank_table_1")
