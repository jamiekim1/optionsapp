#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::fluidPage(
    golem_add_external_resources(),
    # UI logic
    titlePanel("Options "),
    # top of page user input and app intro
    # tabset structure
    shiny::tabsetPanel(
      type = "tabs",
      shiny::tabPanel("How To Use This App"),
      shiny::tabPanel("Options Strategy Builder"),
      shiny::tabPanel("Options Risk Manager"),
      shiny::tabPanel("Options Hedging")
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
      app_title = "optionsapp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
