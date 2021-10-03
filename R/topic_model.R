library(tidyverse)
library(tidytext)
library(topicmodels)

set.seed(2021)

WD <- getwd() %>% 
  gsub(pattern = "nlp-covid.*", replacement = "nlp-covid")

if (!("k" %in% ls())) k <- 2:16 # default value

message(k)

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

write_rds(dat_docmatrix, file = str_c(WD, "/data/docmatrix.RDS"))

for (i in k) {
  set.seed(2021)
  message(i)
  
  mod <- LDA(x = dat_docmatrix, k = i)
  save(list = c("mod"), file = str_c(WD, "/data/topic_models/topic_model", i, ".RData"))
}