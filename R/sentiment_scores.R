library(tidyverse)
library(tidytext)

WD <- getwd()

dat <- list.files(str_c(WD, "/data")) %>% 
  keep(~ str_detect(., "dat_")) %>% 
  {str_c(WD, "/data/", .)} %>% 
  map(readRDS) %>% 
  reduce(rbind)

sentiment_scores <- read_delim(str_c(WD, "/data/sentiment_scores.csv"), 
                               ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  select(word, sentiment = my_sentiment_2)

dat %>% 
  select(date, country, text) %>% 
  head(100) %>% 
  unnest_tokens(word, text) %>% 
  left_join(sentiment_scores) %>% 
  na.omit() %>% 
  group_by(date, country) %>% 
  summarise(sentiment = )
