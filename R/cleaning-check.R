library(tidyverse)
library(tidytext)

WD <- getwd()

dat <- list.files(str_c(WD, "/data")) %>% 
  keep(~ str_detect(., "dat_\\d.RDS")) %>% 
  {str_c(WD, "/data/", .)} %>% 
  map(readRDS) %>% 
  reduce(rbind)

dat %>% 
  sample_n(10000) %>% 
  select(country, text) %>% 
  unnest_tokens(token = "sentences", input = "text", output = "sentence") %>% 
  count(country, sentence, sort = T)

sentence_sample %>%
  count(sentence, sort = T)
