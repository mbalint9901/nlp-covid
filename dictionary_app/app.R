library(shiny)
library(shinydashboard)
library(tidyverse)

# RESTART -----------------------------------

# sentence_sample %>%
#   select(word, topic_name) %>%
#   ungroup() %>%
#   left_join(bing_df) %>%
#   write.csv("new_sentiment.csv")
# ------------------------------------------

sentence_sample <- read_rds("sentence_sample.RDS")
bing_freq <- read_rds("bing_freq.RDS")
bing_df <- tidytext::get_sentiments("bing")

new_sentiment_df <- read.csv("new_sentiment.csv") %>% 
  mutate(sentiment = ifelse(sentiment == "NA", NA, sentiment)) %>% 
  replace_na(list(sentiment = "neutral")) %>% 
  select(-1) # remove row number

ui <- dashboardPage(
  dashboardHeader(title = "Words check"),
  dashboardSidebar(
  ),
  dashboardBody(
    box(width = NULL,
        selectInput(choices = bing_freq, label = "Word:", inputId = "selected_word"),
        selectInput(choices = sort(unique(sentence_sample$topic_name)), label = "Topic:", inputId = "selected_topic"),
        uiOutput("new_sentiment")
    ),
    box(
      width = NULL,
      infoBoxOutput("n_occu", width = 4),
      infoBoxOutput("bing_value", width = 4),
      infoBoxOutput("new_sentiment_infobox", width = 4)
    ),
    box(
      width = NULL, title = "Examples sentences",
      tableOutput("sentences")
    )
  )
  
)

server <- function(input, output, session) {
  
  output$sentences <- renderTable({
    sentence_sample %>%
      filter(word  == input$selected_word, topic_name == input$selected_topic) %>%
      ungroup() %>% 
      select(Examples = sentence)
  })
  
  output$n_occu <- renderInfoBox({
    bing_freq %>%
      filter(word  == input$selected_word) %>% 
      pull(n) %>% 
      format(big.mark = " ") %>% 
      infoBox(title = "Frequency in the corpus", icon = icon("list-ol"))
  })
  
  output$bing_value <- renderInfoBox({
    bing_df %>% 
      filter(word == input$selected_word) %>% 
      pull(sentiment) %>% 
      {infoBox(title = "Original bing value", value = ., icon = icon("thermometer-quarter"), 
               color = ifelse(. == "negative", "red", "green"))}
  })
  
  output$new_sentiment <- renderUI({
    input$selelected_word
    input$selected_topic
    
    value <- new_sentiment_df %>% 
      filter(word  == input$selected_word, topic_name == input$selected_topic) %>% 
      pull(sentiment)
    
    selectInput(choices = c("neutral", "positive", "negative"), 
                selected = value,
                label = "New sentiment:", 
                inputId = "new_sentiment")
  })
  
  output$new_sentiment_infobox <- renderInfoBox({
    
    infoBox(title = "New value", value = input$new_sentiment, icon = icon("wpexplorer"), 
            color = ifelse(input$new_sentiment == "negative", "red", "green"))
  })
  
  observeEvent(input$new_sentiment, {
    
    old_value <- new_sentiment_df %>% 
      filter(word  == input$selected_word, topic_name == input$selected_topic) %>% 
      pull(sentiment)
    
    if (old_value != input$new_sentiment) {
      
      out <-new_sentiment_df %>%
        filter(!(word == input$selected_word & topic_name == input$selected_topic)) %>%
        rbind(
          data.frame(
            word = input$selected_word,
            topic_name = input$selected_topic,
            sentiment = input$new_sentiment
          ) 
        )
      
      write.csv(out, "new_sentiment.csv")
      message("re-write csv!")
      
    new_sentiment <<- out
    }
    
    
  })
}

shinyApp(ui, server)