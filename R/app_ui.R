#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage(
      "Swiss Charge",
      theme = bslib::bs_add_variables(
        bslib::bs_theme(bootswatch = "zephyr", primary = "#44ddf9"),
        "navbar-bg" = "#ffac57"
      ),
      tabPanel(
        "Density Map",
        waiter::use_waiter(),
        mod_tab_start_ui("tab_start")
      ),
      tabPanel(
        "Kanton Charts",
        mod_tab_kanton_ui("tab_kanton")
      ),
      tabPanel(
        "Explorer",
        mod_tab_city_ui("tab_city")
      ),
      tabPanel(
        "Rankings",
        mod_tab_rank_ui("tab_rank")
      ),
      tabPanel(
        "About",
        mod_tab_contact_ui("tab_contact")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Swiss Charge"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
