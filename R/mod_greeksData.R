#' greeksData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_greeksData_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::div(class = "center-container", 
               shiny::selectInput(ns("greek"), "Greek", choices = c("Delta" = "delta",
                                                                    "Gamma" = "gamma",
                                                                    "Theta" = "theta",
                                                                    "Vega" = "vega",
                                                                    "Rho" = "rho")),
               shiny::selectInput(ns("position"), "Position", choices = c("Call" = "call",
                                                                          "Put" = "put"))
    ),
    shiny::div(class = "center-container", 
               shiny::textInput(ns("ticker_symbol"), "Ticker Symbol", placeholder = "Enter a Stock Ticker"),
               shiny::textOutput(ns("current_price_display"))
    ),
    shiny::div(class = "center-container", 
               shiny::dateInput(ns("expiration"), "Expiration Date", value = Sys.Date()),
               shiny::numericInput(ns("strike"), "Strike Price", value = 0, min = 0),
               shiny::numericInput(ns("volatility"), "Volatility (SD)", value = 0.15, min = 0, step = 0.01),
               shiny::numericInput(ns("rate"), "Risk Free Rate", value = 0.035, min = 0, step = 0.001)
    ),
    shiny::div(class = "button-container",  
               shiny::actionButton(ns("calculate_btn"), "Calculate")
    ),
    shiny::br(),
    shiny::br(),
    shiny::div(class = "center-container large-graph-container",  
               plotly::plotlyOutput(ns("greeks_plot"))
    )
  )
}
    
#' greeksData Server Functions
#'
#' @noRd 
mod_greeksData_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Observe the Calculate button
    shiny::observeEvent(input$calculate_btn, {
      # Check all necessary inputs
      req(input$ticker_symbol, input$expiration, input$strike, input$position, input$greek, input$volatility, input$rate)
      
      # Initialize current_price
      current_price <- NULL
      
      # Attempt to fetch the current price and handle possible errors
      tryCatch({
        stock_data <- tidyquant::tq_get(input$ticker_symbol,
                                        get = "stock.prices",
                                        from = Sys.Date() - 5,
                                        to = Sys.Date())
        if (nrow(stock_data) == 0) {
          shiny::showNotification("No data returned for the ticker symbol. Please input a valid ticker.", type = "error", duration = 5)
          return()  
        }
        current_price <- stock_data %>%
          dplyr::filter(date == max(date)) %>%
          dplyr::pull(adjusted)
      }, error = function(e) {
        shiny::showNotification(paste("Error fetching data for ticker symbol:", input$ticker_symbol, ". Error message:", e$message), type = "error", duration = 5)
        return()
      })
      
      req(current_price)  # Ensure current_price is successfully fetched before proceeding
      
      output$current_price_display <- renderText({
        paste("Current Price:", format(current_price, nsmall = 2))
      })
      
      cp <- current_price
      req(cp)
      
      ttm <- (lubridate::yday(input$expiration) - lubridate::yday(Sys.Date())) / 365
      
      range_factor <- if (input$greek == "delta") {
        seq(0.8 * cp, 1.2 * cp, by = 0.01 * cp)
      } else if (input$greek == "gamma") {
        seq(0.8 * cp, 1.2 * cp, by = 0.01 * cp)
      } else if (input$greek == "theta") {
        seq(from = ttm, to = 0, by = -(1 / 365))
      } else if (input$greek == "vega") {
        seq(0.8 * input$volatility, 1.2 * input$volatility, by = 0.01 * input$volatility)
      } else if (input$greek == "rho") {
        seq(0.8 * input$rate, 1.2 * input$rate, by = 0.01 * input$rate)
      }
      
      range_factor <- data.frame(range_factor)
      
      greeks_data <- range_factor
      
      for(i in 1:nrow(range_factor)) {
        modified_input <- range_factor[i, 1]
        
        # Perform Greek calculation
        if(input$position == "call") {
          
          greek_value <- if(input$greek == "delta") {
            roptions::call.greek(input$greek,
                                 modified_input,
                                 input$strike,
                                 ttm,
                                 input$volatility,
                                 input$rate,
                                 d = 0)
            
            } else if(input$greek == "gamma") {
              
            roptions::call.greek(input$greek,
                                 modified_input,
                                 input$strike,
                                 ttm,
                                 input$volatility,
                                 input$rate,
                                 d = 0)
            
            } else if(input$greek == "theta") {
            
            roptions::call.greek(input$greek,
                                 cp,
                                 input$strike,
                                 modified_input,
                                 input$volatility,
                                 input$rate,
                                 d = 0)
            
            } else if (input$greek == "vega") {
              
              roptions::call.greek(input$greek,
                                   cp,
                                   input$strike,
                                   ttm,
                                   modified_input,
                                   input$rate,
                                   d = 0)
              
            } else {
              
              roptions::call.greek(input$greek,
                                   cp,
                                   input$strike,
                                   ttm,
                                   input$volatility,
                                   modified_input,
                                   d = 0)
              
            }
          
        } else {
          
          greek_value <- if(input$greek == "delta") {
            roptions::call.greek(input$greek,
                                 modified_input,
                                 input$strike,
                                 ttm,
                                 input$volatility,
                                 input$rate,
                                 d = 0)
          
          } else if(input$greek == "gamma") {
            
            roptions::call.greek(input$greek,
                                 modified_input,
                                 input$strike,
                                 ttm,
                                 input$volatility,
                                 input$rate,
                                 d = 0)  
          
          } else if(input$greek == "theta") {
            
            roptions::call.greek(input$greek,
                                 cp,
                                 input$strike,
                                 modified_input,
                                 input$volatility,
                                 input$rate,
                                 d = 0)
            
          } else if (input$greek == "vega") {
            
            roptions::call.greek(input$greek,
                                 cp,
                                 input$strike,
                                 ttm,
                                 modified_input,
                                 input$rate,
                                 d = 0)
            
          } else {
            
            roptions::call.greek(input$greek,
                                 cp,
                                 input$strike,
                                 ttm,
                                 input$volatility,
                                 modified_input,
                                 d = 0)
            
          }
          
        }
        
        # Store Greek value
        greeks_data$Greek[i] <- greek_value
      }
      
      r$greeks_data <- data.frame(greeks_data)
      
      
      output$greeks_plot <- plotly::renderPlotly({
        
        greeks_data <- r$greeks_data
        
        greeks_data <- greeks_data %>% 
          dplyr::mutate(Greek = as.numeric(Greek))
        
        subsets <- list()
        
        # Creating subsets of greeks_data
        for (i in 1:nrow(greeks_data)) {
          
          subsets[[i]] <- greeks_data[1:i, ]
          
        }
        
        greek_subset <- data.frame()
        for (i in seq_along(subsets)) {
          subset <- subsets[[i]]
          subset$Frame <- as.factor(i)  # Add frame column
          greek_subset <- rbind(greek_subset, subset)
        }
        
        # Create animated plot
        p <- plotly::plot_ly(greek_subset, 
                             x = ~range_factor, 
                             y = ~Greek, 
                             color = I("blue"), 
                             frame = ~Frame, type = 'scatter', mode = 'lines+markers',
                     line = list(shape = 'spline')) %>%
          plotly::layout(
            title = paste("Change in", input$greek),
            xaxis = list(title = "Factor"),
            yaxis = list(title = input$greek)) %>%
          plotly::animation_opts(frame = 100, redraw = TRUE) %>%
          plotly::animation_slider(currentvalue = list(prefix = "Frame: ")) %>%
          plotly::animation_button(x = 0.5, xanchor = "center", y = -0.2, yanchor = "top")

        p
        
        
      })
      
    }, ignoreNULL = TRUE)
    
    
  })
}
    

## To be copied in the UI
# mod_greeksData_ui("greeksData_1")
    
## To be copied in the server
# mod_greeksData_server("greeksData_1")
