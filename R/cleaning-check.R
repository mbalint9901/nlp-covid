library(tidyverse)
library(tidytext)

WD <- getwd()

dat <- list.files(str_c(WD, "/data")) %>% 
  keep(~ str_detect(., "dat_\\d+.RDS")) %>% 
  {str_c(WD, "/data/", .)} %>% 
  map(readRDS) %>% 
  reduce(rbind)

dat %>% 
  sample_n(10000) %>% 
  select(country, text) %>% 
  unnest_tokens(token = "sentences", input = "text", output = "sentence") %>% 
  count(country, sentence, sort = T)

dat %>% 
  sample_n(10000) %>% 
  select(country, url, text) %>% 
  unnest_tokens(token = "ngrams", n = 8, input = "text", output = "words") %>% 
  count(words, sort = T)

.Last.value %>% 
  head(200) %>% 
  View()

ToClean <- c("Download NewsGuardInstall our plugin to see the NewsGuard icons in search engine results and in Facebook, Twitter and LinkedIn feeds on your computer browser. Download our new mobile app available for iOS and Android.",
             "Click here to read the newsletter in your browser and subscribe", 
             "your browser does not allow viewing this content")

dat %>% 
  filter(str_detect(text, "browser"), str_detect(text, "allow"), str_detect(text, "viewing")) %>% 
  mutate(text = str_remove_all(text, "Your browser does not allow viewing of this content.")) %>% 
  mutate(text = str_remove_all(text, "Your browser does not allow viewing this content.")) %>% 
  mutate(text = str_remove_all(text, "Your browser does not allow you to view this content.")) %>% 
  filter(str_detect(text, "browser"), str_detect(text, "allow"), str_detect(text, "viewing")) %>% 
  pull(text) %>% 
  print()
