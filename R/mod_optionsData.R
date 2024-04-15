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
    # Center the text input, date input, select input, and button
    shiny::div(class = "center-container",
               shiny::textInput(ns("ticker_symbol"), "Ticker Symbol", placeholder = "Enter the ticker symbol"),
               shiny::dateInput(ns("expiration_date"), "Expiration Date"),
               shiny::selectInput(ns("option_type"), "Option Type", choices = c("Call" = "call", "Put" = "put"))
    ),
    shiny::div(class = "center-container",
               shiny::actionButton(ns("generate_btn"), "Generate")),
    # Center the options table output
    shiny::div(class = "center-container",
               DT::dataTableOutput(ns("options_table"))),
    
    shiny::div(class = "center-container",
               shiny::numericInput(ns("minStrike"), "Minimum Strike", value = 0),
               shiny::numericInput(ns("maxStrike"), "Maximum Strike", value = 200)),
    
    # Center the plot outputs
    shiny::div(class = "center-container",
               plotly::plotlyOutput(ns("stock_graph")),
               plotly::plotlyOutput(ns("bidask_graph")),
               plotly::plotlyOutput(ns("vol_graph")))
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
                     # Get option chain data from quantmod package
                     quantmod::getOptionChain(input$ticker_symbol, Exp = input$expiration_date)
                   }, error = function(e) {
                     # Display the specific error message to the user
                     errorMsg <- e$message  # Extract the error message from the error object
                     shiny::showNotification(paste("Error fetching options data:", errorMsg),
                                             type = "error",
                                             duration = 5)  # Duration in seconds
                     return(NULL)  # Return NULL to avoid further processing with a non-existent `optionChain`
                   })
                   
                   # Process the data only if it was successfully fetched
                   if (!is.null(r$optionChain)) {
                     # Decide whether to display calls or puts based on the user's selection
                     r$df <- if (input$option_type == "call") {
                       r$optionChain[[1]]
                     } else {
                       r$optionChain[[2]]
                     }
                     
                     r$df <- r$df %>%
                       dplyr::select(Strike, Bid, Ask, Vol, OI, IV, ITM) %>% 
                       dplyr::mutate(IV = round(IV, 2),
                                     Spread = (round((((Ask - Bid) / Ask)) * 100, 2) ))
                     
                   }
                 }, ignoreNULL = TRUE)
                 
                 output$options_table <- DT::renderDataTable({
                   req(r$df)  # Ensure r$df is not NULL before attempting to render the DataTable
                   DT::datatable(r$df, options = list(pageLength = 5))
                   
                 })
                 
                 observeEvent(input$generate_btn, {
                   req(input$ticker_symbol)
                   
                   # Calculate the date 5 years ago from today
                   startDate <- Sys.Date() - (5 * 365)
                   
                   # Fetch historical stock price data
                   stockData <- tryCatch({
                     # Get stock price data using tidyquant package
                     tidyquant::tq_get(input$ticker_symbol,
                                       get = "stock.prices",
                                       from = Sys.Date() - (365 * 5),
                                       to = Sys.Date())
                     
                   }, error = function(e) {
                     # Display the specific error message to the user
                     errorMsg <- e$message  # Extract the error message from the error object
                     shiny::showNotification(paste("Error fetching options data:", errorMsg),
                                             type = "error",
                                             duration = 5)
                     return(NULL)
                   })
                   
                   # Check if stockData is not NULL before proceeding
                   req(!is.null(stockData))
                   
                   output$stock_graph <- plotly::renderPlotly({
                     p1 <- plotly::plot_ly(data = stockData, x = ~date, y = ~adjusted,
                                           type = 'scatter', mode = 'lines') %>%
                       plotly::layout(title = paste("5 Year Stock Price for", toupper(input$ticker_symbol)),
                                      xaxis = list(title = "Date"),
                                      yaxis = list(title = "Adjusted Close Price"))
                     
                     p2 <- plotly::plot_ly(data = stockData, x = ~date, y = ~volume,
                                           type = "bar") %>% 
                       plotly::layout(yaxis = list(title = "Volume",
                                                   side = "right"))
                     plotly::subplot(p1, p2, nrows = 2, 
                                     heights = c(0.7, 0.1), 
                                     shareX = TRUE) %>% 
                       plotly::layout(showlegend = FALSE)
                   })
                   
                   
                 })
                 observeEvent(input$generate_btn, {
                   
                   r$df_filtered <- reactive({
                     req(r$df)
                     r$df %>%
                       dplyr::filter(Strike >= input$minStrike & Strike <= input$maxStrike)
                   })
                   
                   output$bidask_graph <- plotly::renderPlotly({
                     req(r$df_filtered())
                     plotly::plot_ly(data = r$df_filtered(),
                                     x = ~Strike,
                                     y = ~Spread,
                                     type = 'scatter',
                                     mode = 'lines') %>% 
                       plotly::layout(title = 'Bid-Ask Spread Over Multiple Strikes',
                                      xaxis = list(title = 'Strike'),
                                      yaxis = list(title = 'Bid-Ask Spread (%)'))
                   })
                   
                   
                   
                   output$vol_graph <- plotly::renderPlotly({
                     plotly::plot_ly(data = r$df_filtered(),
                                     x = ~Strike,
                                     y = ~Vol,
                                     type = "bar") %>% 
                       plotly::layout(title = "Volume Over Multiple Strikes",
                                      xaxis = list(title = "Strike"),
                                      yaxis = list(title = "Volume"))
                   })
                   
                 })
               })
}


    
## To be copied in the UI
# mod_optionsData_ui("optionsData_1")
    
## To be copied in the server
# mod_optionsData_server("optionsData_1")
