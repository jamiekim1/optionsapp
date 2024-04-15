#' sentimentAnalysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sentimentAnalysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::div(class = "center-container",
               shiny::textInput(ns("ticker"), "Ticker Symbol", placeholder = "Enter a Stock Ticker"),
               shiny::actionButton(ns("analyze_btn"), "Perform Analysis")
    ),
    shiny::br(),
    shiny::column(4, plotly::plotlyOutput(ns("recent_sentiment"))),
    shiny::div(class = "center-container", 
                 shiny::column(8, shiny::plotOutput(ns("recent_wordcloud")))
    ),
    shiny::br(),
    shiny::br(),
    shiny::column(4, plotly::plotlyOutput(ns("year_sentiment"))),
    shiny::div(class = "center-container",
                 shiny::column(8, shiny::plotOutput(ns("year_wordcloud")))

    )
  )
}
    
#' sentimentAnalysis Server Functions
#'
#' @noRd 
mod_sentimentAnalysis_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    perform_sentiment_analysis <- function(summaries) {
      dplyr::tibble(text = summaries) %>%
        tidytext::unnest_tokens(word, text) %>%
        dplyr::inner_join(tidytext::get_sentiments("bing"), by = "word") %>%
        dplyr::count(sentiment) %>%
        tidyr::spread(sentiment, n, fill = 0) %>%
        dplyr::mutate(sentiment = dplyr::if_else(positive > negative, "Positive", "Negative"))
    }
    
    preprocess_text <- function(text) {
      text <- tolower(text)
      text <- stringr::str_remove_all(text, "[[:punct:]]")
      text <- stringr::str_remove_all(text, "\\d")
      text <- stringr::str_squish(text)
      
      # Remove specific phrases
      additional_phrases <- c("investorsobserver", "prnewswire", "pricewatch")
      phrases_pattern <- paste(additional_phrases, collapse = "|")
      
      combined_pattern <- sprintf("\\b(%s|%s)\\b", paste(tm::stopwords("en"), collapse = "|"), phrases_pattern)
      
      text <- stringr::str_remove_all(text, combined_pattern)
      return(text)
    }
    
    shiny::observeEvent(input$analyze_btn, {
      
      req(input$ticker)
      
      website1 <- paste0("https://www.bloomberg.com/search?query=", input$ticker)
      
      response1 <- httr::GET(website1, httr::add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"))
      page1 <- rvest::read_html(httr::content(response1, type = "text"))
      
      summaries1 <- page1 %>%
        rvest::html_nodes(".summary__a759320e4a") %>%
        rvest::html_text()
      
      website2 <- paste0("https://www.insidertracking.com/company-news?ticker=", input$ticker)
      
      response2 <- httr::GET(website2, httr::add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"))
      page2 <- rvest::read_html(httr::content(response2, type = "text"))
      
      summaries2 <- page2 %>% 
        rvest::html_nodes(".abstract") %>%
        rvest::html_text()
      
      if (length(summaries1) == 0 || length(summaries2) == 0) {
        shiny::showNotification("No data found for the ticker symbol provided", type = "message")
        return()
      }
      
      
      r$sentiment_recent <-  perform_sentiment_analysis(summaries1) %>%
        dplyr::select(-sentiment) %>%
        tidyr::pivot_longer(cols = everything(), names_to = "Sentiment", values_to = "Count")
      
      r$sentiment_year <-  perform_sentiment_analysis(summaries2) %>%
        dplyr::select(-sentiment) %>%
        tidyr::pivot_longer(cols = everything(), names_to = "Sentiment", values_to = "Count")
      
      output$recent_sentiment <- plotly::renderPlotly({
        
        plotly::plot_ly(r$sentiment_recent, labels = ~Sentiment, values = ~Count, type = 'pie', textinfo = 'percent',
                        insidetextorientation = 'radial') %>% 
          plotly::layout(title = "Recent Sentiment (Past Couple Months)")
        
      })
      
      output$year_sentiment <- plotly::renderPlotly({
        
        plotly::plot_ly(r$sentiment_year, labels = ~Sentiment, values = ~Count, type = 'pie', textinfo = 'percent',
                        insidetextorientation = 'radial') %>% 
          plotly::layout(title = " Sentiment Over the Past Year")
        
      })

      summaries1 <- preprocess_text(summaries1)
      corpus1 <- tm::Corpus(tm::VectorSource(summaries1))
      
      
      # Create a term-document matrix
      tdm1 <- tm::TermDocumentMatrix(corpus1)
      
      # Convert term-document matrix to a data frame
      tdm_matrix1 <- as.matrix(tdm1)
      word_counts1 <- sort(rowSums(tdm_matrix1), decreasing = TRUE)
      df_word_counts1 <- data.frame(word = names(word_counts1), freq = word_counts1)
      
      output$recent_wordcloud <- shiny::renderPlot({
        # Create an empty plot where the word cloud will be placed
        plot.new()
        plot.window(xlim = c(0.5, 1.5), ylim = c(0.5, 1.5), asp = 1)
        
        # Generate the word cloud
        wordcloud::wordcloud(words = df_word_counts1$word, 
                             freq = df_word_counts1$freq, 
                             min.freq = 1,
                             scale = c(2.2, 1), 
                             max.words = 100, 
                             random.order = FALSE, 
                             rot.per = 0.35,
                             colors = RColorBrewer::brewer.pal(8, "Dark2"))
        
        # Add a title to the plot
        title(main = "Recent Word Cloud", cex.main = 2)
      })
      
      summaries2 <- preprocess_text(summaries2)
      corpus2 <- tm::Corpus(tm::VectorSource(summaries2))
      
      # Create a term-document matrix
      tdm2 <- tm::TermDocumentMatrix(corpus2)
      
      # Convert term-document matrix to a data frame
      tdm_matrix2 <- as.matrix(tdm2)
      word_counts2 <- sort(rowSums(tdm_matrix2), decreasing = TRUE)
      df_word_counts2 <- data.frame(word = names(word_counts2), freq = word_counts2)
      
      output$year_wordcloud <- shiny::renderPlot({
        # Create an empty plot where the word cloud will be placed
        plot.new()
        plot.window(xlim = c(0.5, 1.5), ylim = c(0.5, 1.5), asp = 1)
        
        # Generate the word cloud
        wordcloud::wordcloud(words = df_word_counts2$word, 
                             freq = df_word_counts2$freq, 
                             min.freq = 1,
                             scale = c(2.2, 1), 
                             max.words = 100, 
                             random.order = FALSE, 
                             rot.per = 0.35,
                             colors = RColorBrewer::brewer.pal(8, "Dark2"))
        
        # Add a title to the plot
        title(main = "Yearly Word Cloud", cex.main = 2)  # Adjust title text size as needed
      })


      
    })
 
  })
}
    
## To be copied in the UI
# mod_sentimentAnalysis_ui("sentimentAnalysis_1")
    
## To be copied in the server
# mod_sentimentAnalysis_server("sentimentAnalysis_1")
