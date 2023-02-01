#' kanton_gt_stations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_kanton_gt_stations_ui <- function(id){
  ns <- NS(id)
  div(
    gt::gt_output(ns("stations")),
    downloadLink(ns("downloadData"), "Download"),
    tags$hr()
  )
}

#' kanton_gt_stations Server Functions
#'
#' @noRd
mod_kanton_gt_stations_server <- function(id, data_in){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    data_out <- reactive({
      data_in() %>%
        dplyr::count(kanton, wappen_url, name = "stations", sort = TRUE) %>%
        dplyr::mutate(kanton = stringr::str_replace_na(
          kanton,
          replacement = "None Listed"
        )) %>%
        dplyr::left_join(kanton_pop_data, by = "kanton") %>%
        dplyr::mutate(ratio = pop_21 / stations,
                      ratio_bar = ratio,
                      # Add in Swiss wappen for "None Listed"
                      wappen_url = stringr::str_replace_na(
                        wappen_url,
                        stringr::str_glue("https://upload.wikimedi\\
                        a.org/wikipedia/commons/thumb/9/95/Coat_of_arms_o\\
                        f_Switzerland.svg/1082px-Coat_of_arms_of_Switzerlan\\
                                          d.svg.png")
                        )
        ) %>%
        dplyr::select(wappen_url,
                      kanton,
                      population = pop_21,
                      stations,
                      ratio,
                      ratio_bar) %>%
        dplyr::arrange(ratio) %>%
        dplyr::mutate(kanton = forcats::fct_inorder(kanton))
    })

    gt_table <- gt::render_gt({
      data_out() %>%
        gt::gt() %>%
        gt::tab_header(
          title = "Kanton Details",
          subtitle = "Residents per charging station"
        ) %>%
        gt::text_transform(
          locations = gt::cells_body(c(wappen_url)),
          fn = function(x) {
            gt::web_image(
              url = x,
              height = "20px"
            )
          }
        ) %>%
        gtExtras::gt_plt_bar(column = ratio_bar,
                             keep_column = FALSE,
                             color = "darkgrey") %>%
        gt::cols_label(
          ratio = gt::html("residents<br>per<br>station"),
          ratio_bar = "",
          wappen_url = ""
        ) %>%
        gt::cols_align(kanton, align = "left") %>%
        gt::sub_missing(missing_text = "-") %>%
        gt::fmt_number(
          columns = c("ratio", "population"),
          decimals = 0,
          sep_mark = "'"
        ) %>%
        gt::tab_footnote(
          footnote = "Charging stations missing Kanton label.",
          locations = gt::cells_body(columns = kanton,
                                     rows = kanton == "None Listed")
        ) %>%
        gt::opt_row_striping() %>%
        gtExtras::gt_theme_538()
    })

    output$stations <- gt_table

    output$downloadData <- downloadHandler(
      filename = function() {
        stringr::str_glue(
          "Kanton Details {Sys.Date()}.csv"
        )

      },
      content = function(file) {
        write.csv(data_out() %>%
                    dplyr::select(.data = ., -wappen_url, -ratio_bar),
                  file, row.names = FALSE)
      }
    )

  })
}

## To be copied in the UI
# mod_kanton_gt_stations_ui("kanton_gt_stations_1")

## To be copied in the server
# mod_kanton_gt_stations_server("kanton_gt_stations_1")
