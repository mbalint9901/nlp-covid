library(tidyverse)
library(tidytext)
library(topicmodels)
library(stm)

set.seed(2021)

WD <- getwd() %>% 
  gsub(pattern = "nlp-covid.*", replacement = "nlp-covid")

if (!("k" %in% ls())) k <- c(17:19, seq(20,60,by=5)) # default value

message(k)

dat <- list.files(str_c(WD, "/data")) %>% 
  keep(~ str_detect(., "dat_\\d+.RDS")) %>% 
  {str_c(WD, "/data/", .)} %>% 
  map(readRDS) %>% 
  bind_rows()

# dat_docmatrix <- dat %>% 
#   group_by(country) %>% 
#   group_modify(~ sample_n(.x, 1000, replace = FALSE)) %>% 
#   ungroup() %>% 
#   transmute(r = row_number(), text) %>% 
#   unnest_tokens(word, text) %>%
#   #anti_join(get_stopwords()) %>%
#   filter(!str_detect(word, "[0-9]+")) %>%
#   add_count(word) %>%
#   filter(n > 100) %>%
#   select(-n)
# 
# dat_docmatrix <- dat_docmatrix %>%
#   count(r, word) %>%
#   cast_sparse(r, word, n)

dat_docmatrix <- readRDS(file = str_c(WD, "/data/docmatrix.RDS"))

for (i in k) {
  message(i)
  mod <- stm(dat_docmatrix, K = i)
  save(list = c("mod"), file = str_c(WD, "/data/topic_models/stm_topic_model", i, ".RData"))
}
