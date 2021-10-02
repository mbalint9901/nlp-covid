library(tidyverse)
library(tidytext)
library(topicmodels)

set.seed(2021)

WD <- getwd() %>% 
  gsub(pattern = "nlp-covid.*", replacement = "nlp-covid")

dat <- list.files(str_c(WD, "/data")) %>% 
  keep(~ str_detect(., "dat_\\d+.RDS")) %>% 
  {str_c(WD, "/data/", .)} %>% 
  map(readRDS) %>% 
  bind_rows()

dat_docmatrix <- dat %>% 
  group_by(country) %>% 
  group_modify(~ sample_n(.x, 1000, replace = FALSE)) %>% 
  ungroup() %>% 
  transmute(r = row_number(), text) %>% 
  unnest_tokens(output = words, input = text) %>% 
  count(r, words, sort = T) %>% 
  cast_dfm(r, words, n)

for (i in 2:16) {
  message(i)
  mod <- LDA(x = dat_docmatrix, k = i, control = list(seed = 2021))
  save(list = c("mod"), file = str_c(WD, "/data/topic_models/topic_model", i, ".RData"))
}