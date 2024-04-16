#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    shiny::fluidPage(
      theme = shinythemes::shinytheme("sandstone"),
      golem_add_external_resources(),
      shiny::div(class="title-panel", 
                 titlePanel("TradeWise: Options")
      ),
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel("User Guide",
                        mod_userGuide_ui("userGuide_1")),
        shiny::tabPanel("Options Chain",
                        mod_optionsData_ui("optionsData_1")),
        shiny::tabPanel("Strategy Builder",
                        mod_optionsStrategy_ui("optionsStrategy_1")),
        shiny::tabPanel("Greeks",
                        mod_greeksData_ui("greeksData_1")),
        shiny::tabPanel("Sentiment Analysis",
                        mod_sentimentAnalysis_ui("sentimentAnalysis_1"))
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
      app_title = "optionsapp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
