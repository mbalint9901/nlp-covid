library(tidyverse)
library(tidytext)

dat <- all_news()

dat_words_monthly <- dat %>% 
  select(country, date, text) %>% 
  mutate(
    date = str_c(lubridate::year(date), "-",lubridate::month(date)),
    date = lubridate::ym(date)
  ) %>% 
  unnest_tokens(words, text, "words") %>% 
  count(country, date, words)


saveRDS(dat_words_monthly, "data/dat_words_monthly.RDS")