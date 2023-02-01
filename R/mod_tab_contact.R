#' tab_contact UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_contact_ui <- function(id){
  ns <- NS(id)
  tagList(
    headerPanel(
      title = "About Swiss Charge",
    ),
    sidebarLayout(
      mainPanel(
        div(
          includeMarkdown("inst/app/md/about.md")
        ),
        div(
          includeMarkdown("inst/app/md/thanks.md")
        )
      ),
      sidebarPanel(
        div(
          includeMarkdown("inst/app/md/contact.md"),
          actionButton(ns("email"),
                       label = "Email",
                       icon = icon("envelope")
          )
          ),
        div(
          includeMarkdown("inst/app/md/data.md")
        ),
        div(
          includeMarkdown("inst/app/md/programmed.md")
        )
      ),
    )
  )
}

#' tab_contact Server Functions
#'
#' @noRd
mod_tab_contact_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$email, {
      updateActionButton(inputId = "email",
                         label = stringr::str_glue(
                           "<a href='mailto:swisschar\\
                           geapp@g\\
                           mail.com?sub\\
                           ject=Swiss Charge App Contact'>swiss\\
                           chargeapp@gm\\
                           ail.com</a>"
                         )
                         )
    })
  })
}

## To be copied in the UI
# mod_tab_contact_ui("tab_contact")

## To be copied in the server
# mod_tab_contact_server("tab_contact")
