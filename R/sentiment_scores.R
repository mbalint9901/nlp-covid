library(tidyverse)
library(tidytext)

WD <- getwd() %>% 
  gsub(pattern = "nlp-covid.*", replacement = "nlp-covid")

setwd(WD)

dat <- list.files(str_c(WD, "/data")) %>% 
  keep(~ str_detect(., "dat_\\d+.RDS")) %>% 
  {str_c(WD, "/data/", .)} %>% 
  map(readRDS) %>% 
  reduce(rbind)

dat_topics <- readRDS(str_c(WD, "/data/dat_topics.RDS"))

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

sent_dictionary <- read.csv(str_c(WD, "/data/modified_sentiment_dictionary.csv"), sep = ";") %>% 
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

for (i in 1:length(unique(dat_joined$date))) { 
  # not enogh memory -> perform the calculation iterativly by splitting the df
  message(i)
  dat_i <- dat_joined %>% 
    select(-title, -top_topic, -URL) %>% 
    pivot_longer(-(1:5)) %>% 
    #mutate(group = cut(as.numeric(id), 100, F)) %>%
    filter(month == unique(dat_joined$date)[i]) %>% 
    #filter(group == i) %>%
    transmute(id, date, month, country, topic = as.numeric(str_remove(name, "topic_")), text, topic_ratio = value) %>% 
    unnest_tokens("word", text) %>% 
    #filter(!is.na(word)) %>% 
    left_join(sent_dictionary)
  
  dat_sentiment_article <- 
    dat_i %>% 
    group_by(id) %>% 
    #weighted mean with weights fromm topic_n
    summarise(sent_mean = weighted.mean(x = sent, w=topic_ratio, na.rm = T), sent_n = sum(!is.na(sent))/12, n = n()/12) %>%  # TODO
    ungroup() %>% 
    mutate(sent_mean = ifelse(is.nan(sent_mean) == T, 0, sent_mean)) %>% 
    left_join(dat_joined) %>% 
    select(-text) %>% 
    bind_rows(dat_sentiment_article)
  
  dat_sentiment_daily <- 
    dat_i %>% 
    group_by(date, country) %>% 
    # Maradhat a sima átlag (?)
    #summarise(n = sum(n), sent_mean = sum(sent_mean*sent_n) / sum(sent_n), sent_n = sum(sent_n)) %>% 
    summarise(sent_mean = weighted.mean(x = sent, w=topic_ratio, na.rm = T), 
              daily_weight = sum(topic_ratio[!is.na(sent)]),
              sent_n = sum(!is.na(sent))/12, 
              n = n()/12) %>%  # TODO
    ungroup() %>% 
    bind_rows(dat_sentiment_daily)
  
}

#Daily sentiment calculation inside function

  dat_sentiment_monthly <- 
    dat_i %>%
    group_by(month, country) %>%
    summarise(sent_mean = weighted.mean(x = sent, w=topic_ratio, na.rm = T), sent_n = sum(!is.na(sent))/12, n = n()/12) %>%  # TODO
    ungroup() %>%
    bind_rows(dat_sentiment_monthly)

save(
  list = c("dat_sentiment_article", "dat_sentiment_daily", "dat_sentiment_monthly"), 
  file = str_c(WD, "/data/sentiment_scores_results.RData")
)