library(shiny)
library(shinydashboard)
library(tidyverse)

sentence_sample <- read_rds("sentence_sample.RDS")


ui <- dashboardPage(
  dashboardHeader(title = "Words check"),
    dashboardSidebar(
      selectInput(choices = sort(unique(filter(sentence_sample, !is.na(bing_sentiment) & bing_sentiment != new_sentiment)$words)), label = "Word:", inputId = "selected_word")
  ),
  dashboardBody(
    box(width = NULL,
                        infoBoxOutput("n_occu", width = 6)
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
    filter(words  == input$selected_word) %>%
    select(Examples = sentences)
  })
  
  output$n_occu <- renderInfoBox({
    sentence_sample %>%
    filter(words  == input$selected_word) %>% 
      nrow() %>%
      {./nrow(sentence_sample)} %>%
      {scales::percent(., accuracy = .01)}%>% 
      infoBox(title = "Rate of affected sentences")
  }
  )
}

shinyApp(ui, server)