library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(parallel)
library(geofacet)
library(tidytext)
library(tm)

WD <- getwd()
setwd(str_c(WD, "/data/raw & temporary/"))

# greece --------------------------------------------------------

greece <- readxl::read_excel("greece.xlsx")

for (i in 1:nrow(greece)) {
  if (!is.na(greece[i, 1])) {
    title <- greece[i, 1]
    date <- greece[i, 2]
    URL <- greece[i, 3]
    text <- greece[i, 4]
    n_missing <- 0
    
  } else{
    greece[i, 1] <- title
    greece[i, 2] <- date
    greece[i, 3] <- URL
    greece[i, 4] <- str_c(text, greece[i, 4], collapse = " ")
    text <- greece[i, 4]
  }
}

greece <- greece %>% 
  mutate(
    r = 1:nrow(greece)
  ) %>% 
  arrange(desc(r)) %>% 
  filter(!duplicated(URL) & !is.na(text) & str_length(text) != 0) %>% 
  mutate(
    date2 = lubridate::mdy(date),
    text = str_remove_all(text, '\"'),
    text = gsub("Photo: ", "", text),
    text = str_remove_all(text, 'Share the article:'),
    text = gsub("Reportage-text-photo: ", "", text),
    text = str_remove_all(text, 'Source:*'),
    text = str_remove_all(text, 'SOURCE'),
    text = str_remove_all(text, 'απε'),
    text = str_remove_all(text, 'ΑΠΕ'),
    text = str_remove_all(text, 'ΜΠΕ'),
    text = str_remove_all(text, 'ΕRT'),
    text = str_remove_all(text, 'ERT1'),
    text = str_remove_all(text, 'AFP'),
    text = str_remove_all(text, 'Reuters'),
    text = gsub('Related news.*', '', text),
    text = gsub('Share this article', '', text),
    date2 = ifelse(is.na(date2), as.character(lubridate::dmy(date)), as.character(date2)),
    date = ifelse(is.na(date2), as.character(as.Date(as.numeric(date), 
                                                     origin = "1899-12-30")),
                  as.character(date2)),
    date = lubridate::ymd(date)
  ) %>% 
  select(-r, -date2)


# norway --------------------------------------------------------

norway <- readxl::read_excel("norway.xlsx") %>%
  mutate(
    text = str_remove_all(text, '\"'),
    date = lubridate::mdy(date)
  )


# bulgaria ------------------------------------------------------

bulgaria <- readxl::read_excel("bulgaria.xlsx") %>% 
  mutate(date = lubridate::mdy(date))


# finland -------------------------------------------------------

finland <- read_excel("finland.xlsx")

for (i in 1:nrow(finland)) {
  if (!is.na(finland[i, 1])) {
    title <- finland[i, 1]
    date <- finland[i, 2]
    URL <- finland[i, 3]
    text <- finland[i, 4]
    n_missing <- 0
    
  } else{
    finland[i, 1] <- title
    finland[i, 2] <- date
    finland[i, 3] <- URL
    finland[i, 4] <- str_c(text, finland[i, 4], collapse = " ")
    text <- finland[i, 4]
  }
}

finland <- finland %>% 
  mutate(
    r = 1:nrow(finland)
  ) %>% 
  arrange(desc(r)) %>% 
  filter(!duplicated(URL) & !is.na(text) & str_length(text) != 0) %>% 
  mutate(
    date = lubridate::mdy(date),
    text = str_remove_all(text, '(another service)') # TODO other formats
  ) %>% 
  select(-r) %>% 
  filter(text != "AS")

finland <- read_excel("finland2.xlsx") %>%
  filter(!is.na(text)) %>%
  mutate(
    date = ifelse(str_detect(date, '-'), date, as.character(as.Date(as.numeric(date), origin = "1899-12-30"))),
    date = ymd(date)
  ) %>% 
  arrange(!is.na(date)) %>%
  rbind(finland) %>% 
  filter(!duplicated(URL))

finland <- finland %>% 
  mutate(
    text = str_remove_all(text, 'you will switch to another service'),
    text = str_remove_all(text, 'you switch to another service'),
    text = str_remove_all(text, 'switch to another service'),
    text = str_remove_all(text, 'you will move to another service'),
    text = str_remove_all(text, 'move to another service'),
    text = str_remove_all(text, 'you move to another service'),
    text = str_remove_all(text, 'moving to another service'),
    text = str_remove_all(text, 'switching to another service'),
    text = str_remove_all(text, 'you will go to another service'),
    text = str_remove_all(text, 'you go to another service'),
    text = str_remove_all(text, 'going to another service'),
    text = str_remove_all(text, 'go to another service'),
    text = str_remove_all(text, 'which moves to another service'),
    text = str_remove_all(text, 'transferred to another service'),
    text = str_remove_all(text, 'at another service'),
    text = str_remove_all(text, 'read on to another service'),
    text = str_remove_all(text, ' to another service'),
    text = str_remove_all(text, '\\(\\)')
  ) 

# france --------------------------------------------------------

france <- read_excel("france.xlsx") %>% 
  mutate(date = paste0('202', gsub('-.*', '', gsub('.*/202', '', URL))),
         date = ymd(date)
  ) 


# iceland -------------------------------------------------------

iceland <- read_excel("iceland.xlsx") %>% 
  mutate(date = lubridate::mdy(date))


# italy ---------------------------------------------------------

f.remove_writer <- function(x) {
  if (str_starts(x, 'by')) {
    spaces <- str_locate_all(x," ")[[1]][,1] 
    spaces <-  ifelse(spaces < str_locate(x," [[:lower:]]")[[1]], spaces, NA)
    str_sub(x, start = max(na.omit(spaces)) + 1)
  } else {
    x
  }
}


italy <- read_excel("italy2.xlsx")


cl <- makeCluster(7)
clusterExport(cl, list("italy", "f.remove_writer"), envir = environment())
clusterEvalQ(cl, library(tidyverse))
italy <- parApply(cl = cl, italy, 1, function(x) {
  tibble(date = x[1], title = x[2], URL = x[3], text = f.remove_writer(x[4]))
})
stopCluster(cl)

italy <- reduce(italy, rbind) %>% 
  tibble() %>% 
  mutate(
    date = case_when( 
      str_detect(date, ', 202') ~ as.character(mdy(date)),
      str_detect(date, '. 202') ~ as.character(dmy(date)),
      str_length(date) == 5 ~ as.character(as.Date(as.numeric(date), 
                                                   origin = "1899-12-30")),
      T ~ as.character(NA)
    ),
    date = ymd(date),
    text = paste0(gsub('.*-', '', str_sub(text, end = 20)), str_sub(text, start = 21)),
  ) %>% 
  mutate(text = str_remove_all(text, "kwait.*[);\\]]{3}")) %>% 
  mutate(text = str_remove_all(text, "kwait.*pl_listen")) %>% 
  mutate(text = str_remove_all(text, "kwait.*pm_list")) %>% 
  mutate(text = str_remove_all(text, "kwait.*[aA]dref *="))


# malta ---------------------------------------------------------

malta <- read_excel("malta.xlsx") %>% 
  mutate(date = lubridate::mdy(date)) %>% 
  mutate(
    text = str_remove_all(text, str_c(c('See also', 'tvm.com.mt'), collapse="|"))
  )


# czech -----------------------------------------------------------------------------
czech <- read_excel("czech.xlsx") %>% 
  mutate(
    date2 = dmy(date),
    date3 = as.character(as.Date(as.numeric(date), origin = "1899-12-30")),
    date4 = mdy(date),
    
    date = ifelse(is.na(date2), 
                  ifelse(is.na(date3),
                         as.character(date4),
                         as.character(date3)),
                  as.character(date2)),
    date = ymd(date)
  ) %>% select(-c(date2, date3, date4))


# portugal --------------------------------------------------------------------------

Sys.setlocale("LC_ALL","English")
portugal <- read_excel("portugal.xlsx") %>% 
  mutate(
    date2 = as.Date(date, format="%d %B %Y"),
    date3 = as.Date(date, format="%B %d, %Y"),
    date4 = as.Date(as.numeric(date), origin = "1899-12-30"),
    date5 = as.Date(str_remove(date, "[pP]ublished"), format="%d %B %Y"),
    date = ifelse(is.na(date2), 
                  ifelse(is.na(date3),
                         ifelse(is.na(date4),
                                as.character(date5),
                                as.character(date4)),
                         as.character(date3)),
                  as.character(date2)),
    date = ymd(date)
  ) %>% filter(!is.na(date)) %>% filter(!is.na(text)) %>% 
  mutate(text = str_remove_all(text, "\\.var.*() *;")) %>% 
  select(-c(date2, date3, date4, date5))

portugal <- read_excel("portugal2.xlsx") %>% 
  mutate(
    date2 = as.Date(date, format="%d %B %Y"),
    date3 = as.Date(date, format="%B %d, %Y"),
    date4 = as.Date(as.numeric(date), origin = "1899-12-30"),
    date5 = as.Date(str_remove(date, "[pP]ublished"), format="%d %B %Y"),
    date = ifelse(is.na(date2), 
                  ifelse(is.na(date3),
                         ifelse(is.na(date4),
                                as.character(date5),
                                as.character(date4)),
                         as.character(date3)),
                  as.character(date2)),
    date = ymd(date)
  ) %>% 
  select(-c(date2, date3, date4, date5)) %>% 
  filter(!is.na(date) & !is.na(text)) %>% 
  mutate(text = str_remove_all(text, "\\.var.*() *;"))


# lithuania -------------------------------------------------------------------------

lithuania <- read_excel("lithuania.xlsx") %>% 
  mutate(
    date = mdy(date),
    text = str_remove_all(text, str_c(c('lrt.lt'), collapse="|"))
  )


# sweden ----------------------------------------------------------------------------

sweden <- read_excel("sweden.xlsx") %>% 
  filter(!is.na(text)) %>% 
  mutate(date2 = str_extract(date, "(.*?) at"),
         date = ifelse(is.na(dmy(date2)), as.character(mdy(date2)), 
                       as.character(dmy(date2)))
  ) %>% 
  select(-date2)

# spain -----------------------------------------------------------------------------

spain <- read_excel("spain.xlsx") %>% 
  filter(!is.na(text)) %>% 
  mutate(
    date = ymd(str_extract(Url, "202[0-9]{5}"))
  ) %>% 
  filter(!is.na(date)) %>% 
  rename(URL = Url)

# belgium_french --------------------------------------------------------------------

belgium_french <- read_excel("belgium_french.xlsx") %>% 
  mutate_at(-1, function(x) zoo::na.locf(x)) %>% 
  filter(str_detect(date, '202')) %>% 
  mutate(
    date2 = gsub(".*day,* ", "", date),
    date = ifelse(grepl(",", date2, fixed = TRUE), as.character(mdy(date2)),
                  as.character(dmy(date2)))
  ) %>% 
  select(date, title, URL = Url, text)


# Belgium_dutch ---------------------------------------------------------------------

load("Belgium_dutch_rawtext.RData")

belgium_dutch <- Belgium_dutch_rawtext %>%  
  mutate(
    date = as.Date(str_extract(URL, "202[0-1]{1}.{6}"), format="%Y/%m/%d")
  )

# slovakia --------------------------------------------------------------------------

slovakia <- read_excel("slovakia.xlsx") %>%
  mutate(
    date2 = mdy(date), 
    date3 = ymd(as.character(as.Date(as.numeric(date), origin = "1899-12-30"))),
    date4 = dmy(date),
    
    date = ifelse(is.na(date2), 
                  ifelse(is.na(date3),
                         as.character(date4),
                         as.character(date3)),
                  as.character(date2)),
    date = ymd(date)
  ) %>% 
  mutate(text = str_remove_all(text, "[pP]hoto[ ]*gallery.*();")) %>% 
  mutate(text = str_remove_all(text, "[Jj\\.]wplayer.*();")) %>% 
  select(-c(date2, date3, date4))


# romania ---------------------------------------------------------------------------

romania <- read_excel("romania.xlsx") %>% 
  mutate_at(-1, function(x) zoo::na.locf(x)) %>% 
  filter(date != "_x000D_") %>% 
  mutate(
    date2 = dmy(date),
    date3 = ifelse(is.na(date2), 
                   as.character(mdy(date)), 
                   as.character(date2)),
    date3 = ifelse(is.na(date3), 
                   as.character(as.Date(as.numeric(date), origin = "1899-12-30")),
                   as.character(date3)),
    date = ymd(date3),
    text = str_remove_all(text, 'Source')
  ) %>% select(-date2, -date3) 


# denmark ---------------------------------------------------------------------------

load("C:/rprojects/CoronaSentiment/scrapping RData/Denmark_rawtext.RData")
denmark <-  read_excel("denmark.xlsx") %>% 
  merge(select(Denmark_rawtext, date_orig = date, URL)) %>% 
  tibble() %>% 
  mutate(
    date = ifelse(str_detect(date_orig, 'maj'), paste0('5-', gsub('. maj.*', '', date_orig), '-2020'), date),
    date = mdy(date)
  ) %>% 
  select(-date_orig)

# croatia ---------------------------------------------------------------------------

croatia <- read_excel("croatia.xlsx") %>% 
  mutate(
    date2 = mdy(date),
    date = ifelse(is.na(date2), 
                  as.character(as.Date(as.numeric(date), origin = "1899-12-30")),
                  as.character(date2)),
    date = ymd(date)
  ) %>% select(-date2)

# austria ---------------------------------------------------------------------------

austria <- read_excel("austria.xlsx") %>% 
  mutate(
    date = mdy(date),
    text = str_remove_all(text, '[( ][Aa][Pp][Aa][) ]'),
    text = str_remove_all(text, '[( ][Dd][Pp][Aa][) ]'),
    text = str_remove_all(text, '[( ][Aa][Ff][Pp][) ]'),
    text = str_remove_all(text, '[( ][Rr]euters[) ]')
  ) %>% rename(URL = Url)

# slovenia --------------------------------------------------------------------------

slovenia <- read_excel("slovenia.xlsx")  %>% 
  mutate(
    date2 = mdy(date),
    date = ifelse(is.na(date2), 
                  as.character(as.Date(as.numeric(date), origin = "1899-12-30")),
                  as.character(date2)),
    date = ymd(date)
  ) %>% select(date, title, URL, text)


# switzerland -----------------------------------------------------------------------

load("Switzerland_rawtext.RData")

switzerland <- Switzerland_rawtext %>% 
  mutate(
    text = str_remove_all(text, 'This article was automatically imported from our old content management system. If you see any display errors, please let us know: community-feedback@swissinfo.ch')
  ) %>% 
  tibble()

# hungary ---------------------------------------------------------------------------

hungary <- read_excel("hungary.xlsx") %>% 
  mutate(
    date = as.Date(date, format= "%Y. %m. %d."),
    text = str_remove_all(text, 'mtva_player[^;]*;'),
    text = str_remove_all(text, 'Cover photo[:]* [A-Za-z]*')
  ) %>% 
  select(date, title, URL, text)


# cyprus ----------------------------------------------------------------------------

load("C:/rprojects/CoronaSentiment/scrapping RData/Cyprus_rawtext.RData")
cyprus <- Cyprus_rawtext %>% 
  tibble() %>% 
  mutate(
    date = mdy(date)
  )

# estonia ---------------------------------------------------------------------------

estonia <- read_excel("estonia.xlsx") %>% 
  tibble() %>% 
  mutate(
    date = mdy(date)
  )

# poland ----------------------------------------------------------------------------

poland <- read_excel("poland.xlsx") %>% 
  tibble() %>% 
  mutate(
    date = ymd(date),
    title = 'poland',
    text = text_final
  ) %>% 
  select(date, title, URL, text) %>% 
  mutate(text = str_remove_all(text, "U FFFD")) %>% 
  mutate(text = str_remove_all(text, "U FEFF")) %>% 
  mutate(text = str_remove_all(text, "function.*640px")) %>% 
  mutate(text = str_remove_all(text, "[KC]ORONA[VW]IRUS R[EI]PORT")) %>% 
  mutate(text = str_remove_all(text, "[KC]ORONA[VW]IRUS[\\.]* R[EI]PORT"))

# ireland ---------------------------------------------------------------------------

load("Ireland_rawtext.RData")

ireland <- Ireland_rawtext %>% 
  tibble() %>% 
  mutate(
    date = gsub('..:..', '', date),
    date = mdy(date)
  )

# germany ---------------------------------------------------------------------------

germany <- read_excel("germany.xlsx") %>% 
  tibble() %>% 
  mutate(
    date = gsub('2021.*', '2021', date),
    date = gsub('2020.*', '2020', date),
    date = ifelse(str_count(date, '\\.') == 2,
                  as.character(dmy(date)), as.character(mdy(date))),
    date = ymd(date)
  )

# latvia ----------------------------------------------------------------------------

latvia <- read_excel("latvia.xlsx") %>% 
  mutate(date = mdy(date))

# uk --------------------------------------------------------------------------------

load("uk_rawtext.RData")
uk <- uk_rawtext %>% 
  mutate(date = dmy(date))

# luxemburg -------------------------------------------------------------------------

luxemburg <- read_excel("luxemburg.xlsx") %>% 
  transmute(
    date = ymd(Date),
    title = Title,
    URL = Url,
    text = Text
  )


# netherlands -----------------------------------------------------------------------

netherlands <- read_excel("netherlands.xlsx") %>% 
  mutate(
    date2 = ifelse(str_detect(date, '2020'), as.character(dmy(gsub(', .*', '', date))), 
                   paste0('2021', case_when(
                     str_detect(date, 'Jan') ~ '-1-',
                     str_detect(date, 'Feb') ~ '-2-',
                     str_detect(date, 'Mar') ~ '-3-',
                     str_detect(date, 'Apr') ~ '-4-',
                     str_detect(date, 'May') ~ '-5-',
                     str_detect(date, 'Jun') ~ '-6-',
                     str_detect(date, 'Jul') ~ '-7-',
                     str_detect(date, 'Aug') ~ '-8-',
                     str_detect(date, 'Sep') ~ '-9-',
                     str_detect(date, 'Oct') ~ '-10-',
                     str_detect(date, 'Nov') ~ '-11-',
                     str_detect(date, 'Dec') ~ '-12-',
                     T ~ as.character(NA)
                   ), str_remove_all(gsub('..:..', '', date), '[^\\d]')
                   )
    ),
    date3 = ifelse(as.numeric(str_remove_all(str_sub(date2, end = 7), '[^\\d]')) > 20213, str_replace_all(date2, '2021', replacement = '2020'), date2),
    date3 = ymd(date3)
  ) %>% 
  select(date = date3, title, URL, text) %>% 
  filter(!is.na(date) & !is.na(text))

# merge ---------------------------------------------------------

dat <- greece %>% mutate(country = "EL") %>% 
  rbind(mutate(norway, country = "NO")) %>% 
  rbind(mutate(bulgaria, country = "BG")) %>% 
  rbind(mutate(finland, country = "FI")) %>% 
  rbind(mutate(france, country = "FR")) %>% 
  rbind(mutate(iceland, country = "IS")) %>% 
  rbind(mutate(italy, country = "IT")) %>% 
  rbind(mutate(malta, country = "MT")) %>% 
  rbind(mutate(czech, country = "CZ")) %>% 
  rbind(mutate(portugal, country = "PT")) %>% 
  rbind(mutate(lithuania, country = "LT")) %>% 
  rbind(mutate(sweden, country = "SE")) %>% 
  rbind(mutate(spain, country = "ES")) %>% 
  rbind(mutate(belgium_french, country = "BE_french")) %>% 
  rbind(mutate(belgium_dutch, country = "BE_dutch")) %>% 
  rbind(mutate(slovakia, country = "SK")) %>% 
  rbind(mutate(romania, country = "RO")) %>% # TODO filter words
  rbind(mutate(denmark, country = "DK")) %>% 
  rbind(mutate(croatia, country = "HR")) %>% 
  rbind(mutate(austria, country = "AT")) %>%
  rbind(mutate(slovenia, country = "SI")) %>%
  rbind(mutate(switzerland, country = "CH")) %>%
  rbind(mutate(hungary, country = "HU")) %>%
  rbind(mutate(cyprus, country = "CY")) %>%
  rbind(mutate(estonia, country = "EE")) %>%
  rbind(mutate(poland, country = "PL")) %>%
  rbind(mutate(ireland, country = "IE")) %>%
  rbind(mutate(germany, country = "DE")) %>%
  rbind(mutate(latvia, country = "LV")) %>%
  rbind(mutate(uk, country = "UK")) %>%
  rbind(mutate(luxemburg, country = "LU")) %>%
  rbind(mutate(netherlands, country = "NL")) %>%
  mutate(
    text = gsub('https.* ', '', text),
    text = gsub('pic.twitter.com.* ', '', text),
    text = str_replace_all(text, '\"', " "),
    text = str_replace_all(text, '«', " "),
    text = str_replace_all(text, '»', " "), 
    title = str_replace_all(title, '\"', " ") # TODO more special characters
  ) %>% 
  filter(
    !is.na(text) & text != "" & str_length(text) > 20
  )

for (i in 2:(nrow(dat)-1)) { # imputing missing date values
  if (!is.na(dat$date[i])) {
    last_date <- dat$date[i]
  } else {
    dat[i, 1] <- dat %>% # if the following known date equals to the previous known one,
      tail(-i) %>%                               # then imput, in other case leave it
      filter(!is.na(dat$date[i])) %>% 
      .[1, ] %>% 
      pull(date) %>% 
      {ifelse(. == last_date, last_date, NA)}
  }
}

dat <- dat %>% 
  filter(!is.na(dat$date[i])) %>% 
  filter(date < lubridate::ymd("2021-02-01") &
           date > lubridate::ymd("2019-12-31")
  )


dat <- dat %>% 
  mutate(country = ifelse(str_detect(country, "BE"), "BE", country))


# add sentiment ---------------------------------------------------------------------

setwd("C:/rprojects/CoronaSentiment")

dat_sentiment_daily <- tibble()  
dat_sentiment_monthly <- tibble()  
dat_words_monthly <- tibble()  

sen_lex_nrc <- get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  mutate(value = ifelse(sentiment == "positive", 1, -1)) %>% 
  select(word, value)

sen_bing <- get_sentiments("bing") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  mutate(value = ifelse(sentiment == "positive", 1, -1)) %>% 
  select(word, value)

modified_bing <- read_excel("bing_to_score.xlsx") %>% 
  select(word, value = 'my sentiment') %>% 
  mutate(value = as.numeric(value)) %>% 
  na.omit()

for (i in seq_along(unique(pull(dat, country)))) {
  
  dat_sentiment <- dat %>% 
    filter(country == unique(pull(dat, country))[i]) %>% 
    select(date, text, country) %>% 
    {left_join(unnest_tokens(., words, text), 
               modified_bing, by=c("words"="word"))}  
  
  dat_sentiment_daily <- rbind(
    dat_sentiment_daily,
    dat_sentiment %>% 
      group_by(date, country) %>% 
      summarise(sentiment = mean(value, na.rm = T), n_total = n(), n = sum(!is.na(value))) %>% 
      ungroup() %>% 
      na.omit() %>% 
      mutate(country = unique(pull(dat, country))[i])
  )
  
  dat_sentiment_monthly <- rbind(
    dat_sentiment_monthly,
    dat_sentiment %>% 
      na.omit() %>% 
      mutate(
        date = lubridate::ym(paste(lubridate::year(date), lubridate::month(date), sep = "-"))
      ) %>% 
      group_by(date, country) %>% 
      summarise(sentiment = mean(value, na.rm = T), n_total = n(), n = sum(!is.na(value))) %>% 
      ungroup() %>% 
      na.omit() %>% 
      mutate(country = unique(pull(dat, country))[i])
  )
  
  dat_words_monthly <- rbind(
    dat_words_monthly,
    dat_sentiment %>% 
      select(-value) %>% 
      mutate(
        date = lubridate::ym(paste(lubridate::year(date), lubridate::month(date), sep = "-"))
      ) %>% 
      group_by(date, words) %>% 
      summarise(n = n()) %>% 
      ungroup() %>% 
      mutate(country = unique(pull(dat, country))[i])
  )
  
}


# save ------------------------------------------------------------------------------

setwd(str_c(WD, "/data"))

dat %>% 
  mutate(
    r = row_number(),
    r = cut(r, breaks = 6, labels = F)
  ) %>% 
  group_by(r) %>% 
  group_map(.keep = T, ~ saveRDS(select(.x, -r), str_c("dat_", first(.x$r), ".RDS")))



# save(list = c('dat', 'dat_sentiment_daily', 'dat_sentiment_monthly',  'dat_covid_monthly', 'dat_words_monthly', 'dat_covid', 'Hungary_rawtext', 'dat_covid_monthly', 'dat_eco_sent', 'dat_unemployment', 'mygrid'), 
     # file = "C:/rprojects/CoronaSentiment/datda.RData")

setwd(WD)
