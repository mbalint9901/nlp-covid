library(tidyverse)

dat_sentiment_daily <- readRDS("data/dat_sentiment_daily.RDS")

dat_sentiment_daily %>%
  select(1:5) %>% 
  pivot_longer(- c(1:2)) %>% 
  ggplot() + 
  aes(date, value, color = name) + 
  geom_line() +
  facet_wrap(~ country)

dat_sentiment_daily %>% 
  select(1:5) %>% 
  pivot_longer(- c(1:2)) %>% 
  filter(country == "UK") %>% 
  ggplot() + 
  aes(date, value, color = name) + 
  geom_line(size = 1.3)

dat_sentiment_daily %>% 
  filter(country == "UK") %>% 
  ggplot() +
  aes(date, n) + 
  geom_col()

dat <- list.files("data/") %>% 
  keep(~ str_detect(., "dat_\\d.RDS")) %>% 
  {str_c("data/", .)} %>% 
  map(readRDS) %>% 
  reduce(rbind)

dat %>% 
  filter(country == "UK") %>% 
  count(date) %>% 
    ggplot() + 
    aes(date, n) + 
    geom_col()
  
