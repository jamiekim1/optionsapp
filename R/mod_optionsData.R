#' optionsData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_optionsData_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::textInput(ns("ticker_symbol"), "Ticker Symbol", placeholder = "Enter the ticker symbol"),
    shiny::numericInput(ns("strike_price"), "Strike Price", value = NULL, min = 0, step = 0.01),
    shiny::dateInput(ns("expiration_date"), "Expiration Date"),
    shiny::selectInput(ns("option_type"), "Option Type", choices = c("Call" = "call", "Put" = "put")),
    shiny::actionButton(ns("generate_btn"), "Generate"),
    DT::dataTableOutput(ns("options_table")) # Placeholder for the table
  )
}
    
#' optionsData Server Functions
#'
#' @noRd 
mod_optionsData_server <- function(id, r) {
  
  moduleServer(id, 
               function(input, output, session) {
                 observeEvent(input$generate_btn, {
                   # Ensure all inputs are available before proceeding
                   req(input$ticker_symbol, input$expiration_date)
                   
                   # Fetch options data based on user input, with error handling
                   r$optionChain <- tryCatch({
                     quantmod::getOptionChain(input$ticker_symbol, Exp = input$expiration_date)
                   }, error = function(e) {
                     # Display an error notification to the user
                     shiny::showNotification("Error fetching options data. Please check the ticker symbol and try again.",
                                             type = "error",
                                             duration = 5)  # Duration in seconds, adjust as needed
                     return(NULL)  # It's important to return NULL to avoid further processing with a non-existent `optionChain`
                   })
                   
                   # Process the data only if it was successfully fetched
                   if (!is.null(r$optionChain)) {
                     # Decide whether to display calls or puts based on the user's selection
                     r$df <- if (input$option_type == "call") {
                       r$optionChain[[1]]
                     } else {
                       r$optionChain[[2]]
                     }
                   }
                 }, ignoreNULL = TRUE)
                 
                 output$options_table <- DT::renderDataTable({
                   req(r$df)  # Ensure r$df is not NULL before attempting to render the DataTable
                   DT::datatable(r$df)
                   
                 })
    })
}


    
## To be copied in the UI
# mod_optionsData_ui("optionsData_1")
    
## To be copied in the server
# mod_optionsData_server("optionsData_1")
