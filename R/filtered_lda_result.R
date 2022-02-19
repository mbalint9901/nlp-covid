library(tidyverse)
library(tidytext)
library(topicmodels)

WD <- getwd() %>% 
  gsub(pattern = "nlp-covid.*", replacement = "nlp-covid")

topic_model_df <- list.files("data/filtered_topic_models/", full.names = TRUE) %>% 
  keep(str_detect, "RData") %>% 
  {set_names(., str_remove_all(., "\\D"))} %>% 
  {.[as.character(2:15)]} %>% 
  map(~ {load(.); get("mod")}) %>% 
  enframe(value = "model", name = "n_topics") %>% 
  mutate(n_topics = as.numeric(n_topics))

topic_model_df %>% 
  mutate(
    l = map_dbl(model, logLik)
  ) %>% 
  ggplot(aes(n_topics, l)) + 
  geom_line()

ap_topics <- topic_model_df %>% 
  filter(n_topics == 12) %>% 
  pull(model) %>% 
  .[[1]] %>% 
  tidy()

ap_topics %>% 
  anti_join(select(stop_words, term = word)) %>%
  group_by(topic) %>% 
  slice_max(beta, n = 30) %>% 
  arrange(desc(beta)) %>% 
  summarise(words = str_c(term, collapse = ", ")) %>% 
  mutate(
    name = c("lockdown", "school", "politics", "health", "vaccine research", "sport", "global politics", "vaccine politics", "mental health", "General statistics", "Global statistics", "Economy"),
    words
  )

library(tidyr)

beta_wide <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic5 > .001 | topic8 > .001) %>%
  mutate(log_ratio = log2(topic5 / topic8))

beta_wide %>% 
  slice_max(abs(log_ratio), n = 30) %>% 
  mutate(term = fct_reorder(term, log_ratio)) %>% 
  ggplot(aes(log_ratio, term)) + 
  geom_col()


set.seed(2022)

document_df <- dat %>% 
  group_by(country) %>% 
  sample_n(1000) %>%
  ungroup() %>% 
  transmute(document = as.character(row_number()), country, date, text)


document_df <- topic_model_df %>% 
  filter(n_topics == 12) %>% 
  pull(model) %>% 
  .[[1]] %>% 
  tidy(matrix = "gamma") %>% 
  left_join(x = document_df)

topic_name <- function(x) {
c("Restrictions", "Schools", "General politics", "Hospitals", "Vaccine research", "Sport", "Global politics", "Vaccine politics", "Mental health",  "General statistics", "Global statistics", "Economy")[[as.numeric(x)]]
}

document_df %>% 
  group_by(date, topic) %>% 
  summarise(gamma = mean(gamma)) %>% 
  ungroup() %>% 
  filter(topic == 10 | topic == 11) %>% 
  ggplot(aes(date, gamma, color = as.factor(topic))) + 
  geom_line() + 
  geom_smooth()

document_df %>% 
  group_by(country, topic) %>% 
  summarise(gamma = mean(gamma)) %>% 
  ungroup() %>% 
  filter(topic == 10 | topic == 11) %>% 
  pivot_wider(names_from = topic, values_from = gamma) %>% 
  mutate(
    d = `10` - `11`,
    country = fct_reorder(country, d)
    ) %>% 
  ggplot() + 
  aes(d, country) + 
  geom_col()


document_df %>% 
  group_by(document) %>% 
  slice_max(gamma) %>% 
  ungroup() %>% 
  filter(topic == 10 | topic == 11) %>% 
  select(topic, text) %>% 
  unnest_tokens(words, text) %>% 
  count(topic, words) %>% 
  bind_tf_idf(term = words, document = topic, n = n) %>% 
  filter(n > 50) %>% 
  group_by(topic) %>% 
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>% 
  mutate(words = fct_reorder(words, tf_idf)) %>% 
  ggplot(aes(tf_idf, words, fill = as.factor(topic))) + 
  geom_col(color = "black") + 
  facet_wrap(~ topic, scales = "free_y")
