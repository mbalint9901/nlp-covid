library(tidyverse)
library(tidytext)

WD <- getwd() %>% 
  gsub(pattern = "nlp-covid.*", replacement = "nlp-covid")

dat <- list.files(str_c(WD, "/data/")) %>% 
  keep(~ str_detect(., "dat_\\d.RDS")) %>% 
  {str_c(WD, "/data/", .)} %>% 
  map(readRDS) %>% 
  reduce(rbind)

bing_df <- get_sentiments("bing")


load(str_c(WD, "/data/topics_bydat.RData"))

topic_name <- tibble(
  topic = 1:12, topic_name = c(
    "Researches", "Politics", "Statistics", "Public Life", "Public Institutions", 
    "Economy", "Restrictions", "Vaccination", "Sport", "Travel", "Police Measures", 
    "Mental Health"
  )
)

sentence_sample <- dat %>% 
  left_join(select(dat_topics, URL, topic = top_topic)) %>% 
  left_join(topic_name) %>% 
  select(topic_name, text) %>% 
  sample_frac(1) %>% # ensure randomness
  # FIXME cannot allocate vector of size 1.3 Gb
  unnest_tokens(sentence, text, "sentences") %>% 
  unnest_tokens(word, sentence, drop = FALSE) %>% 
  right_join(bing_df) %>% 
  group_by(topic_name, word) %>% 
  group_modify(~ head(.x, 10)) %>% 
  select(topic_name, word, sentence) %>% 
  ungroup()

saveRDS(sentence_sample, str_c(WD, "/dictionary_app/sentence_sample.RDS"))

bing_freq <- readRDS(str_c(WD, "/data/dat_words_monthly.RDS")) %>% 
  rename(word = words) %>% 
  right_join(bing_df) %>% 
  group_by(word) %>% 
  summarise(n = sum(n, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(n))

saveRDS(bing_freq, str_c(WD, "/dictionary_app/bing_freq.RDS"))
