library(tidyverse)
library(tidyverse)
library(tidytext)
library(topicmodels)

all_news <- function(country_filter = NULL, date_filter = NULL) {
  out <- map(1:12, ~ read_rds(str_c("data/dat_", ., ".RDS"))) %>% 
    bind_rows() %>% 
    arrange(country, date)
  # filters if given
  if (!is.null(country_filter)) out <- filter(out, country %in% country_filter)
  if (!is.null(date_filter)) out <- filter(out, date %in% date_filter)
  out
}
