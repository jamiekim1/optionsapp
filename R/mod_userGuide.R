#' userGuide UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_userGuide_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::div(class = "center-container",
    shiny::selectInput(ns("guideSelection"), "Select a Tab to Learn More:",
                       choices = c("Options Chain", "Strategy Builder", "Greeks", "Sentiment Analysis"))
    ),
    
    shiny::uiOutput(ns("dynamicGuide"))
 
  )
}
    
#' userGuide Server Functions
#'
#' @noRd 
mod_userGuide_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    shiny::observeEvent(input$guideSelection, {
      output$dynamicGuide <- shiny::renderUI({
        shiny::req(input$guideSelection)
        if(input$guideSelection == "Options Chain") {
          return(
            shiny::div(
              shiny::h4(strong("About the Options Chain Tab")),
              shiny::p("The 'Options Chain' tab facilitates the exploration of options data for a specific stock and expiration date. Users can effortlessly query the 
                       options chain by entering the ticker symbol, selecting the type of options (calls or puts), and setting the expiration date. The tab offers advanced 
                       filters like minimum and maximum strike prices to refine results further. This tool is essential for traders and investors seeking detailed insights 
                       into options market dynamics, including liquidity measures such as bid-ask spreads and trading volumes."),
              shiny::p("Additionally, the tab provides interactive visualizations including a stock price graph over the past five years, a bid-ask spread graph, and a volume 
                       graph across different strikes, enhancing the decision-making process through detailed, visual data representations."),
              shiny::h4(strong("How to Use the 'Options Chain' Tab")),
              shiny::p("Navigate the 'Options Chain' tab using these steps to maximize its utility:"),
              shiny::p("1. Enter the ticker symbol of the stock and select the expiration date using the date picker."),
              shiny::p("2. Choose 'Call' or 'Put' to specify the type of options you are interested in."),
              shiny::p("3. Click 'Generate' to fetch the options chain data, which will then be displayed in a table format along with interactive graphs depicting historical stock prices, bid-ask spreads, and volume metrics."),
              shiny::p("4. Utilize the minimum and maximum strike filters to refine the x axis range of the liquidity graphs."),
              shiny::p("5. Explore the interactive graphs for a comprehensive view of historical stock performance and current market conditions. These visuals can help in assessing the volatility and liquidity of the options."),
              shiny::h4(strong("Disclaimers")),
              shiny::p("Data fetched is dependent on the availability from the source and may experience delays or inaccuracies. Always verify with multiple sources before making trading decisions."),
              shiny::p("Ensure correct symbols and dates are entered as the query is sensitive to such details.")
            )
          )
        } else if(input$guideSelection == "Strategy Builder") {
          return(
            shiny::div(
              shiny::h4(strong("About the Options Strategy Tab")),
              shiny::p("The 'Options Strategy' tab enables users to design and analyze customized options strategies. It provides a dynamic platform where traders can construct 
                       various options strategies (such as a long condor) and assess their potential payoffs at expiration. The tab allows for the entry of multiple contract types, such as long calls (LC), 
                       short calls (SC), long puts (LP), and short puts (SP), along with their respective strike prices and premiums."),
              shiny::h4(strong("How to Use the 'Options Strategy' Tab")),
              shiny::p("Utilize the 'Options Strategy' tab effectively with these steps:"),
              shiny::p("1. Enter the ticker symbol of the stock to fetch its current price, which serves as a baseline for the strategy simulation."),
              shiny::p("2. Use the 'Add Row' button to include new contracts into your strategy. Each row represents a different options contract with specified conditions (e.g., type of option, strike price, and premium)."),
              shiny::p("3. Adjust the details of each contract directly in the editable table to fine-tune your strategy."),
              shiny::p("4. Select rows and use the 'Remove Row' button to delete contracts from the strategy."),
              shiny::p("5. Once your strategy is set, click 'Calculate Payoff' to generate a graph displaying the potential financial outcomes across a range of stock prices at expiration."),
              shiny::p("6. Analyze the 'Payoff Graph at Expiration' to understand how the options strategy might perform under various stock price scenarios. This visualization helps in assessing the effectiveness and risk associated with the strategy."),
              shiny::h4(strong("Disclaimers")),
              shiny::p("Position input is case sensitive and should be LC (Long-Call), SC (Short-Call), LP (Long-Put), or SP (Short-Put.")
            )
          )
        } else if(input$guideSelection == "Greeks") {
          return(
            shiny::div(
              shiny::h4(strong("About the Greeks Data Tab")),
              shiny::p("The 'Greeks Data' tab is designed to provide quantitative insights into the sensitivities of options prices relative to various underlying parameters. Users can select from five key Greeks: Delta, Gamma, Theta, Vega, and Rho, 
                       each representing a different aspect of risk and sensitivity in options pricing. By setting the options type (call or put), the stock ticker symbol, and additional parameters such as strike price, volatility, and the risk-free rate, 
                       users can simulate and visualize how these Greeks change in response to shifts in market conditions."),
              shiny::p("This functionality is crucial for traders and analysts who use Greeks to make informed decisions on hedging strategies and to gauge the potential risks and rewards associated with various options strategies."),
              shiny::h4(strong("How to Use the 'Greeks Data' Tab")),
              shiny::p("Navigate the 'Greeks Data' tab effectively with these steps:"),
              shiny::p("1. Select the Greek you wish to analyze from the dropdown menu."),
              shiny::p("2. Choose the type of option (Call or Put) to specify which options pricing model to apply."),
              shiny::p("3. Enter the ticker symbol for the stock and the system will automatically fetch and display the current stock price."),
              shiny::p("4. Set the expiration date, strike price, volatility, and risk-free rate as these parameters influence the sensitivity measurements."),
              shiny::p("5. Click on 'Calculate' to generate a dynamic plot which will display how the selected Greek changes with respect to the input variable over a range, such as stock price movements or time to maturity."),
              shiny::p("6. Click play to see how the greeks change. Analyze the resulting graph to understand how changes in input could affect the Greek and, consequently, the pricing and risk profile of the option."),
              shiny::h4(strong("Disclaimers")),
              shiny::p("The calculations and simulations on this tab are based on theoretical models and assumptions that may not perfectly mirror market realities. Users should treat these results as approximations and are encouraged to perform additional analysis before making trading decisions.")
            )
          )
        } else {
          return(
            shiny::div(
              shiny::h4(strong("About the Sentiment Analysis Tab")),
              shiny::p("The 'Sentiment Analysis' tab provides users with a powerful tool to gauge market sentiment around specific stocks. It analyzes textual data from prominent financial news sources—Bloomberg and Insider Tracking—to 
                       extract and visualize sentiment trends. By entering a stock ticker, users can initiate a comprehensive sentiment analysis, which fetches recent news summaries, processes the text for sentiment, and generates both 
                       pie charts and word clouds to depict the sentiment distribution and frequency of terms used in discussions about the stock."),
              shiny::p("This tab is particularly useful for investors and analysts who wish to understand the market sentiment and its potential impact on stock performance."),
              shiny::h4(strong("How to Use the Sentiment Analysis Tab")),
              shiny::p("Utilize the 'Sentiment Analysis' tab effectively with these steps:"),
              shiny::p("1. Enter the ticker symbol of the stock for which you want to analyze sentiment."),
              shiny::p("2. Click 'Perform Analysis' to start the web scraping and analysis process. The system will retrieve news articles from Bloomberg and Insider Tracking and perform sentiment analysis on the collected summaries."),
              shiny::p("3. Review the 'Recent Sentiment' and 'Year Sentiment' pie charts to understand the proportion of positive and negative sentiments expressed in recent months and over the past year, respectively."),
              shiny::p("4. Examine the 'Recent Word Cloud' and 'Year Word Cloud' to identify key themes and terms frequently mentioned in relation to the stock. These visuals help pinpoint what aspects of the company are currently under the most discussion or scrutiny."),
              shiny::p("5. Use the insights gathered to inform investment decisions or further research on the stock."),
              shiny::h4(strong("Disclaimers")),
              shiny::p("Sentiment analysis is inherently subjective and should be one of several tools used when evaluating stocks. The results depend on the quality and quantity of data available from news sources and may not fully capture all market nuances."),
              shiny::p("Data retrieved is real-time and subject to the accuracy and timeliness of updates from the external websites mentioned. Users should cross-verify with other sources for a comprehensive market analysis."),
              shiny::p("Data sources:"),
              shiny::p("www.bloomberg.com"),
              shiny::p("www.insidertracking.com")
            )
          )
          
          }
      })
    })
  })
}
    
## To be copied in the UI
# mod_userGuide_ui("userGuide_1")
    
## To be copied in the server
# mod_userGuide_server("userGuide_1")
