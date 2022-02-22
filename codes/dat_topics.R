library(tidyverse)
library(tidytext)

set.seed(2021)

load("data/filtered_topic_models/filtered_topic_model12.RData")

mod_topic <- mod

dat <- all_news()

#Posterior estimation and calculation of top topics for each article

v <- seq(from = 1, to = nrow(dat), length.out = 1000) %/% 1

dat_topics <- tibble(
  date = date(),
  country = character(),
  topic_1 = numeric(),
  topic_2 = numeric(),
  topic_3 = numeric(),
  topic_4 = numeric(),
  topic_5 = numeric(),
  topic_6 = numeric(),
  topic_7 = numeric(),
  topic_8 = numeric(),
  topic_9 = numeric(),
  topic_10 = numeric(),
  topic_11 = numeric(),
  topic_12 = numeric()
)

for (i in 1:(length(v) - 1)) {
  message(i)
  dat_topics <- 
    dat[v[i]:v[i + 1], ] %>%
    filter(str_remove_all(text, '\\W') != '') %>% 
    mutate(id = row_number()) %>% 
    unnest_tokens('words', 'text') %>% 
    count(id, words, sort = T) %>% 
    cast_dfm(id, words, n) %>% 
    {topicmodels::posterior(mod_topic, .)} %>% 
    .$topics %>% 
    data.frame() %>% 
    {cbind(select(filter(dat[v[i]:v[i + 1], ], str_remove_all(text, '\\W') != ''),
                  date, country, URL), .)} %>% 
    rename_all(.funs = function(x) str_replace_all(x, 'X', 'topic_')) %>%
    {rbind(dat_topics, .)}
}


dat_topics <- dat_topics %>% 
  pivot_longer(cols = -(1:3)) %>% 
  group_by(URL) %>%
  slice_max(order_by = value, n = 1) %>%
  ungroup() %>% 
  transmute(URL, top_topic = as.numeric(str_remove(name, "topic_"))) %>% 
  {left_join(dat_topics, .)}


saveRDS(dat_topics, "data/dat_topics.RDS")