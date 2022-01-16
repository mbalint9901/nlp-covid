library(tidyverse)
library(tidytext)
library(topicmodels)

set.seed(2021)

WD <- getwd() %>% 
  gsub(pattern = "nlp-covid.*", replacement = "nlp-covid")

if (!("k" %in% ls())) k <- 2:16 # Calculate topics for k=2 to 16

message(k)

dat <- list.files(str_c(WD, "/data")) %>% 
  keep(~ str_detect(., "dat_\\d+.RDS")) %>% 
  {str_c(WD, "/data/", .)} %>% 
  map(readRDS) %>% 
  bind_rows()

remove_df <-  readxl::read_excel("data/country_specific_words.xlsx") %>%
  mutate(plural = map(Nationality, str_split, ",")) %>% 
  unnest(plural) %>% 
  mutate(
    plural = map(plural, str_trim),
    plural = map(plural, ~ str_c(., "s")),
    plural = map_chr(plural, str_c, collapse = ", ")
  ) %>% 
  rowwise() %>% 
  transmute(
    country, 
    words = str_c(c_across(-1), collapse = ", "),
    words = map(words, str_split, ","),
    words = map(words, str_trim),
  ) %>% 
  unnest() %>% 
  mutate(words = str_to_lower(words))

set.seed(2022)

dat %>% 
  group_by(country) %>% 
  sample_n(1000) %>%
  ungroup() %>% 
  transmute(r = row_number(), country, text) %>%
  unnest_tokens(output = words, input = text) %>%
  anti_join(remove_df) %>% 
  count(r, words, sort = T) %>% 
  cast_dfm(r, words, n) %>%
  write_rds(file = str_c(WD, "/data/filtered_topic_models/filtered_docmatrix.RDS"))

dat_docmatrix <- readRDS(str_c(WD, "/data/filtered_topic_models/filtered_docmatrix.RDS"))

walk(2:16,  ~ {
  message(.)
  mod <- LDA(x = dat_docmatrix, k = ., control = list(seed = 2022))
  save(list = "mod", file = str_c(WD, "/data/filtered_topic_models/filtered_topic_model", ., ".RData"))
})
