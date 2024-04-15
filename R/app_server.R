#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  r <- reactiveValues()
  mod_userGuide_server("userGuide_1", r = r)
  mod_optionsData_server("optionsData_1", r = r)
  mod_optionsStrategy_server("optionsStrategy_1", r = r)
  mod_greeksData_server("greeksData_1", r = r)
  mod_sentimentAnalysis_server("sentimentAnalysis_1", r = r)
}
