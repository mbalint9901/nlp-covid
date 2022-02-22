library(tidyverse)
library(tidytext)

options(dplyr.summarise.inform = FALSE)

dat <- all_news()

dat_topics <- readRDS("data/dat_topics.RDS")

topic_name <- tibble(
  topic = 1:12, topic_name = c("Restrictions", 
                               "Schools",
                               "General politics",
                               "Hospitals",
                               "Vaccine research",
                               "Sport",
                               "Global politics",
                               "Vaccine politics",
                               "Mental health",
                               "General statistics",
                               "Global statistics", 
                               "Economy")
)

sent_dictionary <- read.csv("data/modified_sentiment_dictionary.csv", sep = ";") %>% 
  tibble() %>% 
  pivot_longer(-1) %>% 
  set_names("word", "topic_name", "sent") %>% 
  mutate(topic_name = str_replace_all(topic_name, "[.]", " ")) %>% 
  left_join(topic_name) %>% 
  select(-topic_name)


#új tábla: cikk id, 12 topik valószínűség (gamma), szó, long verzió, left_join dictionary,
# súlyozott átlag,
# NEM a cikk szinten aggregáltan számoljuk!


dat_joined <- dat %>% 
  left_join(., dat_topics %>% select(-country, -date) %>%  unique(), by = c("URL")) %>% 
  mutate(id = as.character(row_number()),
         month = lubridate::ym(str_c(lubridate::year(date), "-",lubridate::month(date)))) %>% 
  relocate(id, month)# aggregation is faster if grouping variable is chr

dat_sentiment_article <- tibble()
dat_sentiment_daily <- tibble()
dat_sentiment_monthly <- tibble()

# for (i in 1:length(unique(dat_joined$date))) { 
for (current_country in unique(dat_joined$country)) {
  tictoc::tic(msg = current_country) 
  for (current_date in unique(dat_joined$date)) {
    
    # not enogh memory -> perform the calculation iterativly by splitting the df
    
    dat_i <- dat_joined %>% 
      select(-title, -top_topic, -URL) %>% 
      pivot_longer(-(1:5), names_to = "topic", values_to = "gamma") %>% 
      #mutate(group = cut(as.numeric(id), 100, F)) %>%
      filter(date == current_date & country == current_country) %>% 
      mutate(topic = parse_number(topic)) %>% 
      unnest_tokens("word", text) %>% 
      #filter(!is.na(word)) %>% 
      left_join(sent_dictionary, by = c("topic", "word"))
    
    dat_sentiment_article <- 
      dat_i %>% 
      group_by(id) %>% 
      #weighted mean with weights from topic_n
      summarise(sent_mean = weighted.mean(x = sent, w = gamma, na.rm = T), 
                sent_n = sum(!is.na(sent) * gamma) / 12,
                n = n() / 12) %>% 
      ungroup() %>% 
      mutate(sent_mean = ifelse(is.nan(sent_mean) == T, 0, sent_mean)) %>% 
      left_join(dat_joined[c("id", "URL", "date", "country")], by = "id") %>% 
      bind_rows(dat_sentiment_article)
    
    dat_sentiment_daily <- 
      dat_i %>% 
      group_by(date, country) %>% 
      # Maradhat a sima átlag (?)
      #summarise(n = sum(n), sent_mean = sum(sent_mean*sent_n) / sum(sent_n), sent_n = sum(sent_n)) %>% 
      summarise(sent_mean = weighted.mean(x = sent, w=gamma, na.rm = T), 
                sent_n = sum(!is.na(sent) * gamma) / 12,
                n = n()/12) %>%
      ungroup() %>% 
      bind_rows(dat_sentiment_daily)
    
  }
  RPushbullet::pbPost("note", capture.output(tictoc::toc()))
}

#Daily sentiment calculation inside function

dat_sentiment_monthly <- dat_sentiment_daily %>%
  mutate(m = lubridate::ym(str_c(lubridate::year(date), "-",lubridate::month(date)))) %>% 
  group_by(m, country) %>%
  summarise(sent_mean = weighted.mean(x = sent_mean, w=sent_n, na.rm = T), 
            sent_n = sum(sent_n, na.rm = TRUE), 
            n = sum(n, na.rm = TRUE) 
  ) %>% 
  ungroup()

save(
  dat_sentiment_article, dat_sentiment_daily, dat_sentiment_monthly, 
  file = "data/sentiment_scores_results.RData"
)