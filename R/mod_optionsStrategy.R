#' optionsStrategy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_optionsStrategy_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::div(class = "center-container",  # Apply centering class
               shiny::textInput(ns("ticker"), "Stock Ticker Symbol", placeholder = "Enter the stock ticker"),
               shiny::textOutput(ns("current_price_display"))  # Display the current stock price
    ),
    shiny::div(class = "center-container",  # Center the table
               DT::DTOutput(ns("contracts_table"))  # Output for options contracts table
    ),
    shiny::div(class = "button-container",  # Center and space buttons
               shiny::actionButton(ns("add_row"), "Add Contract"),  # Button to add a new contract row
               shiny::actionButton(ns("remove_row"), "Remove Selected Contract(s)"),  # Button to remove selected rows
               shiny::actionButton(ns("calculate_payoff"), "Calculate Payoff")  # Button to calculate and display payoff graph
    ),
    shiny::div(class = "center-container large-graph-container",  # Center the plot output
               plotly::plotlyOutput(ns("payoff_graph"))  # Output for displaying the payoff graph
    )
  )
}

#' optionsStrategy Server Functions
#'
#' @noRd 
mod_optionsStrategy_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive function to fetch current stock price
    current_price <- reactive({
      req(input$ticker)  # Require ticker symbol input before fetching
      
      tryCatch({
        tidyquant::tq_get(input$ticker,  # Fetch stock prices for the last 5 days
                          get = "stock.prices",
                          from = Sys.Date() - 5,
                          to = Sys.Date()) %>% 
          dplyr::filter(date == max(date)) %>%  # Get the latest available price
          dplyr::pull(adjusted)
        
      }, error = function(e) {
        errorMsg <- e$message  # Handle any errors in fetching data
        shiny::showNotification(paste("Error fetching options data:", errorMsg),
                                type = "error",
                                duration = 5)
        return(NULL)
      })
    })
    
    # Render the current price to the UI
    output$current_price_display <- renderText({
      cp <- current_price()
      req(cp)
      paste("Current Price:", format(cp, nsmall = 2))  # Format and display current price
    })
    
    # Initialize and manage the contracts data
    r$contracts_data <- reactiveVal(data.frame(
      Position = c("LC"),
      `Strike Price` = c(100),
      `Option Premium` = c(1)
    ))
    
    # Render the DT table with row selection enabled
    output$contracts_table <- DT::renderDT({
      DT::datatable(r$contracts_data(), selection = 'multiple', editable = TRUE, rownames = FALSE)
    }, server = TRUE)
    
    # Observe add_row button clicks to add a new row
    shiny::observeEvent(input$add_row, {
      current_data <- r$contracts_data()
      new_row <- data.frame(Position = "LC", 
                            `Strike Price` = 100, 
                            `Option Premium` = 1,
                            stringsAsFactors = FALSE)
      r$contracts_data(rbind(current_data, new_row))
    })
    
    # Observe remove_row button clicks to remove selected row
    shiny::observeEvent(input$remove_row, {
      selected_row <- input$contracts_table_rows_selected
      if (length(selected_row) > 0) {
        current_data <- r$contracts_data()
        r$contracts_data(current_data[-selected_row, , drop = FALSE])
      }
    })
    
    # Observe cell edits and update the contracts data accordingly
    shiny::observeEvent(input$contracts_table_cell_edit, {
      info <- input$contracts_table_cell_edit
      req(info) 
      updated_data <- DT::editData(r$contracts_data(), info, rownames = FALSE)
      if (!all(updated_data$Position %in% c("LC", "SC", "LP", "SP"))) {
        shiny::showNotification("Invalid entry in Position column. Please enter only LC, SC, LP, or SP.",
                                type = "error",
                                duration = 5)
        return()
      }
      
      # If validation passes, update the reactive value with the new data
      r$contracts_data(updated_data)
    })
    
    # Create a function to generate payoff graph data
    create_options_strategy_payoff_graph <- function(contracts, stock_price_range) {
      # Expand the data frame to include each stock price for each contract
      expanded_data <- tidyr::expand_grid(
        stock_price = stock_price_range,
        contract = seq_len(nrow(contracts))
      ) %>%
        dplyr::left_join(dplyr::mutate(contracts, contract = seq_len(nrow(contracts))), by = "contract") %>%
        dplyr::select(-contract)
      
      # Calculate payoff for each combination of stock price and contract
      expanded_data <- expanded_data %>%
        dplyr::mutate(payoff = dplyr::case_when(
          Position == "LC" ~ pmax(stock_price - Strike.Price, 0) - Option.Premium,
          Position == "SC" ~ -pmax(stock_price - Strike.Price, 0) + Option.Premium,
          Position == "LP" ~ pmax(Strike.Price - stock_price, 0) - Option.Premium,
          Position == "SP" ~ -pmax(Strike.Price - stock_price, 0) + Option.Premium
        ))
      
      # Aggregate payoff by stock price
      r$strategy_payoff <- expanded_data %>%
        dplyr::group_by(stock_price) %>%
        dplyr::summarize(total_payoff = sum(payoff), .groups = 'drop')
    }
    
    # Observe calculate_payoff button clicks to generate and render the payoff graph
    observeEvent(input$calculate_payoff, {
      req(r$contracts_data())
      cp <- req(current_price())  # Ensure current price is available
      
      stock_price_range <- seq(current_price() * 0.8, current_price() * 1.2, by = 1)
      
      contracts <- data.frame(r$contracts_data())
      
      create_options_strategy_payoff_graph(contracts, stock_price_range)
      
      output$payoff_graph <- plotly::renderPlotly({
        plot <- plotly::plot_ly(r$strategy_payoff, x = ~stock_price, y = ~total_payoff, type = 'scatter', mode = 'lines',
                                line = list(color = 'blue'))
        plot <- plot %>% plotly::layout(title = "Payoff Graph at Expiration",
                                        xaxis = list(title = "Stock Price"),
                                        yaxis = list(title = "Total Payoff"))
        
        return(plot)
      })
      
    })
  })
}
    
## To be copied in the UI
# mod_optionsStrategy_ui("optionsStrategy_1")
    
## To be copied in the server
# mod_optionsStrategy_server("optionsStrategy_1")
