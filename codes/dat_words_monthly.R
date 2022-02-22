library(tidyverse)
library(tidytext)

set.seed(2021)

WD <- getwd() %>% 
  gsub(pattern = "nlp-covid.*", replacement = "nlp-covid")

setwd(WD)

dat <- list.files(str_c(WD, "/data/")) %>% 
  keep(~ str_detect(., "dat_\\d+.RDS")) %>% 
  {str_c(WD, "/data/", .)} %>% 
  map(readRDS) %>% 
  reduce(rbind)

dat_words_monthly <- dat %>% 
  select(country, date, text) %>% 
  mutate(
    date = str_c(lubridate::year(date), "-",lubridate::month(date)),
    date = lubridate::ym(date)
  ) %>% 
  unnest_tokens(words, text, "words") %>% 
  count(country, date, words)


saveRDS(dat_words_monthly, str_c(WD, "/data/dat_words_monthly.RDS"))