library(tidyverse)
library(tidytext)

WD <- getwd() %>% 
  gsub(pattern = "nlp-covid.*", replacement = "nlp-covid")

dat <- list.files(str_c(WD, "/data/")) %>% 
  keep(~ str_detect(., "dat_\\d.RDS")) %>% 
  {str_c(WD, "/data/", .)} %>% 
  map(readRDS) %>% 
  reduce(rbind)

dat %>%
  select(text) %>% 
  unnes

bing_comparison <- read_delim(str_c(WD, "/data/sentiment_scores.csv"), 
                              ";", escape_double = FALSE, trim_ws = TRUE)

load(str_c(WD, "/data/topics_bydat.RData"))

topic_name <- tibble(
  topic = 1:12, topic_name = c(
    "Researches", "Politics", "Statistics", "Public Life", "Public Institutions", 
    "Economy", "Restrictions", "Vaccination", "Sport", "Travel", "Police Measures", 
    "Mental Health"
  )
)

# sentence_sample <- 
  dat %>% 
  left_join(select(dat_topics, URL, topic = top_topic)) %>% 
  left_join(topic_name) %>% 
  group_by(topic_name) %>% 
  group_modify(~ sample_n(.x, 10000, replace = TRUE)) %>% 
  tidytext::unnest_tokens(sentences, text, token = "sentences") %>% 
  distinct()
  tidytext::unnest_tokens(words, sentences, drop = F) %>% 
  left_join(sen_bing) %>% 
  left_join(modified_bing)


sentence_sample %>% 
  count(words, sort = TRUE)

saveRDS(sentence_sample, file = "sentence_sample.RDS")
