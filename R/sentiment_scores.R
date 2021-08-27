library(tidyverse)
library(tidytext)

WD <- getwd()

dat <- list.files(str_c(WD, "/data")) %>% 
  keep(~ str_detect(., "dat_\\d.RDS")) %>% 
  {str_c(WD, "/data/", .)} %>% 
  map(readRDS) %>% 
  reduce(rbind)

sentiment_scores <- read_delim(str_c(WD, "/data/sentiment_scores.csv"), 
                               ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  select(word, 
         original = sentiment,
         old = my_sentiment,
         new = my_sentiment_2
         )

sentiment_functions <- list(
  sentiment = function(x) mean(x, na.rm = T),
  n_total = function(x) length(x),
  n = function(x) length(na.omit(x))
)

dat_sentiment_daily <- dat %>% 
  select(date, country, text) %>%
  unnest_tokens(word, text) %>% 
  left_join(sentiment_scores) %>% 
  select(-word) %>% 
  group_by(date, country) %>% 
  summarise_each(sentiment_functions) %>% 
  ungroup() %>% 
  select(!ends_with("_total") | starts_with("original_n_total")) %>% 
  rename(n = "original_n_total")

saveRDS(dat_sentiment_daily, str_c(WD, "/data/dat_sentiment_daily.RDS"))

dat_sentiment_monthly <- dat_sentiment_daily %>% 
  mutate(date = lubridate::ym(str_sub(date, end = -3))) %>% 
  group_by(date, country) %>% 
  summarise(
    original_sentiment = sum(original_sentiment * original_n) / sum(original_n),
    old_sentiment = sum(old_sentiment * old_n) / sum(old_n),
    new_sentiment = sum(new_sentiment * new_n) / sum(new_n),
    original_n = sum(original_n),
    old_n = sum(old_n),
    new_n = sum(new_n),
    n = sum(n)
  ) %>% 
  ungroup()

saveRDS(dat_sentiment_monthly, str_c(WD, "/data/dat_sentiment_monthly.RDS"))
