library(tidyverse)
load("C:/rprojects/CoronaSentiment/dat.RData")

sen_bing <- tidytext::get_sentiments("bing") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  mutate(bing_sentiment = ifelse(sentiment == "positive", 1, -1)) %>% 
  select(words = word, bing_sentiment)

modified_bing <- readxl::read_excel("C:/rprojects/CoronaSentiment/bing_to_score.xlsx") %>% 
  select(words = word, value = 'my sentiment') %>% 
  mutate(new_sentiment = as.numeric(value)) %>% 
  na.omit()

sentence_sample <- dat %>% 
  sample_n(50000) %>% 
  tidytext::unnest_tokens(sentences, text, token = "sentences") %>% 
  filter(!duplicated(URL)) %>% 
  tidytext::unnest_tokens(words, sentences, drop = F) %>% 
  left_join(sen_bing) %>% 
  left_join(modified_bing)


sentence_sample %>% 
  count(words, sort = TRUE)

saveRDS(sentence_sample, file = "sentence_sample.RDS")
