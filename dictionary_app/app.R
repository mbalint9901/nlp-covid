library(shiny)
library(tidyverse)
sentence_sample <- read_rds("sentence_sample.RDS")


ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Words check"),
    shinydashboard::dashboardSidebar(
      shiny::selectInput(choices = sort(unique(filter(sentence_sample, !is.na(bing_sentiment) & bing_sentiment != new_sentiment)$words)), label = "Word:", inputId = "selected_word")
  ),
  shinydashboard::dashboardBody(
    shinydashboard::box(width = NULL,
                        shinydashboard::infoBoxOutput("n_occu", width = 6)
                        ),
    shinydashboard::box(
      width = NULL, title = "Examples sentences",
  shiny::tableOutput("sentences")
    )
  )

)

server <- function(input, output, session) {
  output$sentences <- shiny::renderTable({
    sentence_sample %>%
    filter(words  == input$selected_word) %>%
    select(Examples = sentences)
  })
  
  output$n_occu <- shinydashboard::renderInfoBox({
    sentence_sample %>%
    filter(words  == input$selected_word) %>% 
      nrow() %>%
      {./nrow(sentence_sample)} %>%
      {scales::percent(., accuracy = .01)}%>% 
      shinydashboard::infoBox(title = "Rate of affected sentences")
  }
  )
}

shinyApp(ui, server)