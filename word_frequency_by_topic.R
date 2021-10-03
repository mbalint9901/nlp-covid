library(tidyverse)
library(tidytext)

WD <- getwd()

dat <- list.files(str_c(WD, "/data")) %>% 
  keep(~ str_detect(., "dat_\\d.RDS")) %>% 
  {str_c(WD, "/data/", .)} %>% 
  map(readRDS) %>% 
  reduce(rbind)

load(str_c(WD, "/data/topics_bydat.RData"))

topic_name <- tibble(
  topic = 1:12, topic_name = c(
    "Researches", "Politics", "Statistics", "Public Life", "Public Institutions", 
    "Economy", "Restrictions", "Vaccination", "Sport", "Travel", "Police Measures", 
    "Mental Health"
  )
)

sent_dictionary <- read.csv(str_c(WD, "/data/modified_sentiment_dictionary.csv"), sep = ";") %>% 
  tibble() %>% 
  pivot_longer(-1) %>% 
  set_names("word", "topic_name", "sent") %>% 
  mutate(topic_name = str_replace_all(topic_name, "[.]", " ")) %>% 
  left_join(topic_name) %>% 
  select(-topic_name) %>% 
  filter(!is.na(topic))

dat_topics %>% 
  transmute(topic = top_topic) %>% 
  cbind(dat)

word_freq <- tibble()

for (i in 1:100) {
  
  word_freq <- dat %>% 
  left_join(dat_topics[c("URL", "top_topic")]) %>% 
  mutate(group = cut(row_number(), 100, F)) %>% 
  filter(group == i) %>% 
  select(text, topic = top_topic) %>% 
  unnest_tokens("word", "text") %>% 
  count(topic, word) %>% 
  right_join(sent_dictionary[c("word", "topic")]) %>% 
  bind_rows(word_freq)
  
  message(i)

}


word_freq %>% 
  group_by(topic, word) %>% 
  summarise(n = sum(n, na.rm = TRUE)) %>% 
  write.csv(file = "word_frequency_by_topic.csv")

# TODO REMOVE LATER

sentiment_scores <- read_delim("data/sentiment_scores.csv", 
                               ";", escape_double = FALSE, trim_ws = TRUE)

word_freq %>% 
  group_by(topic, word) %>% 
  summarise(n = sum(n, na.rm = TRUE)) %>% 
  group_by(topic) %>% 
  arrange(desc(n)) %>% 
  left_join(sent_dictionary) %>% 
  left_join(select(sentiment_scores, word, my_sentiment)) %>% 
  filter(sent != my_sentiment  & topic %in% c(1, 8))
  
  
