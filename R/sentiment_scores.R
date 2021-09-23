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
  select(-topic_name)

dat <- dat %>% 
  left_join(dat_topics[c("URL", "top_topic")]) %>% 
  mutate(id = as.character(row_number())) # aggregation is faster if grouping variable is chr

dat_sentiment <- tibble()

for (i in 1:100) { 
  # not enogh memory -> perform the calculation iterativly by splitting the df
  message(i)
  dat_sentiment <- dat %>% 
    mutate(group = cut(as.numeric(id), 100, F)) %>% 
    filter(group == i) %>% 
    select(id, topic = top_topic, text) %>% 
    unnest_tokens("word", text) %>% 
    left_join(sent_dictionary) %>% 
    group_by(id) %>% 
    summarise(sent_mean = mean(sent, na.rm = T), sent_n = sum(!is.na(sent)), n = n()) %>% 
    left_join(dat) %>% 
    select(-text) %>% 
    bind_rows(dat_sentiment)
  
}

dat_sentiment_daily <- dat_sentiment %>% 
  group_by(date, country) %>% 
  summarise(n = sum(n), sent_mean = sum(sent_mean*sent_n) / sum(sent_n), sent_n = sum(sent_n)) %>% 
  ungroup()

dat_sentiment_monthly <- dat_sentiment %>% 
  mutate(
    m = str_c(lubridate::year(date), "-",lubridate::month(date)),
    m = lubridate::ym(m)
  ) %>% 
  group_by(m, country) %>% 
  summarise(n = sum(n), sent_mean = sum(sent_mean*sent_n) / sum(sent_n), sent_n = sum(sent_n)) %>% 
  ungroup()

save(
  list = c("dat_sentiment", "dat_sentiment_daily", "dat_sentiment_monthly"), 
  file = str_c(WD, "/data/sentiment_scores_results.RData")
)