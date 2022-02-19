library(shiny)
library(shinydashboard)
library(tidyverse)

sentence_sample <- read_rds("sentence_sample.RDS")
bing_freq <- read_rds("bing_freq.RDS")
bing_df <- tidytext::get_sentiments("bing")

modified_sentiment_dictionary <- read_delim("modified_sentiment_dictionary.csv", 
                                            ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  #select(-X14) %>% 
  pivot_longer(-1)

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
  
  
  output$new_sentiment_infobox <- renderInfoBox({
    new_sentiment <- modified_sentiment_dictionary %>% 
      filter(word == input$selected_word, name == input$selected_topic) %>% 
      pull(value)
    
    infoBox(title = "New value", 
            value = case_when(
              new_sentiment == 1 ~ "positive",
              new_sentiment == 0 ~ "neutral",
              new_sentiment == -1 ~ "negative"
            ),
            icon = icon("wpexplorer"), 
            color = case_when (
              new_sentiment == 1 ~ "green",
              new_sentiment == 0 ~ "yellow",
              new_sentiment == -1 ~ "red"
            ))
  })
  
}

shinyApp(ui, server)