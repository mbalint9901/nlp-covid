CoronaSentiment
================
Marcell P. Granát & Bálint Mazzag
2021 04 23

# Setup

``` r
# Set up --------------------------------------------------------------------------------

## Packages ============================================================================= 

library(tidyverse)
library(knitr)
library(broom)
library(geofacet)
library(tidytext)
library(tm)
library(wordcloud)
library(lubridate)
library(knitr)

ggthemr::ggthemr("greyscale")

## Working directory ====================================================================

WD <- getwd() %>% 
  gsub(pattern = "nlp-covid.*", replacement = "nlp-covid")

setwd(WD)
```

# Data

``` r
# Data ----------------------------------------------------------------------------------

# DFs from the cleaning.R file ==========================================================

dat <- list.files(str_c(WD, "/data/")) %>% 
  keep(~ str_detect(., "dat_\\d+.RDS")) %>% 
  {str_c(WD, "/data/", .)} %>% 
  map(readRDS) %>% 
  reduce(rbind)


# Topic models ==========================================================================

# Topic models calculated in -> topic_models.R <-
# moved this calculations to different files due to the large computation time
# Posterior estimation of each article with topic models is also computation heavy
# >> find the estimation in -> dat_topics.R <-

dat_topics <- read_rds(str_c(WD, "/data/dat_topics.rds"))

# Sentiment scores  ==========================================================================

# Sentiment scores are calculated by a dictionary based on topics -> sentiment_scores.R <-
# moved this calculations to different files due to the large computation time

load(str_c(WD, "/data/sentiment_scores_results.RData"))
# 
# dat_sentiment_daily <- read_rds(str_c(WD, "/data/dat_sentiment_daily.RDS")) %>% 
#   select(date, country, sentiment =  new_sentiment, n = new_n, n_total = n)
# 
# dat_sentiment_monthly <- read_rds(str_c(WD, "/data/dat_sentiment_monthly.RDS")) %>% 
#   select(date, country, sentiment = new_sentiment, n = new_n, n_total = n)
# 
dat_words_monthly <- read_rds(str_c(WD, "/data/dat_words_monthly.RDS"))

load(str_c(WD, "/data/raw/Germany_rawtext.RData"))

source(str_c(WD, "/R/data_setup.R")) # additional datasets available online

# This RData contains the articles after the main cleaning process
# To ensure full reproducibility see the attached files at the corresponding
# GitHub Repo: -> https://github.com/MarcellGranat/CoronaSentiment <-


### COVID-dictionary ####################################################################

# own edited sentiment dictionary calibrated to COVID articles

bing_comparison <- read_delim(str_c(WD, "/data/sentiment_scores.csv"), 
                              ";", escape_double = FALSE, trim_ws = TRUE)

modified_bing <- bing_comparison  %>% 
  select(word, value = 'my_sentiment_2') %>% 
  na.omit()

word_frequency <- read_delim(str_c(WD, "/word_frequency_by_topic.csv"), 
                              ",", escape_double = FALSE, trim_ws = TRUE)
```

    ## Error: 'C:/Users/Knipping/Documents/Git_Projects/nlp-covid/word_frequency_by_topic.csv' does not exist.

``` r
metadata_df <- tibble::tribble(
  ~"Country", ~"Country code",            ~"Media outlet", ~"State-financed",     ~"Language",
  "Austria",          "AT",                    "Die Presse",            "No",    "German",
  "Belgium (French)",          "BE",                          "RTBF",           "Yes",  "French",
  "Belgium (Dutch)",          "BE",                           "VRT",           "Yes",  "Dutch",
  "Bulgaria",          "BG", "Bulgarian National Television",           "Yes",   "Bulgarian",
  "Cyprus",          "CY",                   "Cyprus Mail",            "No",    "English",
  "Czech Republic",          "CZ",                "Česká televize",           "Yes",     "Czech",
  "Denmark",          "DK",                     "Politiken",            "No",      "Danish",
  "United Kingdom",          "UK",                  "The Guardian",            "Nem",    "English",
  "Estonia",          "EE",                           "ERR",           "Yes",     "Estonian",
  "Finland",          "FI",                      "Yle News",           "Yes",     "Finnish",
  "France",          "FR",                     "France 24",           "Yes",  "French",
  "Greece",          "EL",                           "ERT",           "Yes",    "Greek",
  "Netherlands",          "NL",                           "NOS",            "No",  "Dutch",
  "Croatia",          "HR",                 "Večernji list",            "No",   "Croatian",
  "Ireland",          "IR",               "The Irish Times",            "No",    "English",
  "Iceland",          "IS",                           "RÚV",           "Yes",  "Icelandic",
  "Poland",          "PL",                           "TVP",           "Yes",  "Polish",
  "Latvia",          "LV",                           "LSM",           "Yes",     "Latvian",
  "Lithuania",          "LT",                "LRT televizija",           "Yes",   "Lithuanian",
  "Luxembourg",          "LU",                   "L'essentiel",            "No",  "French",
  "Hungary",          "HU",                     "hirado.hu",           "Yes",   "Hungarian",
  "Malta",          "MT",                           "TVM",           "Yes",   "Maltese",
  "Germany",          "DE",                   "DER SPIEGEL",            "No",    "German",
  "Norway",          "NO",                           "NRK",           "Yes",   "Norwegian",
  "Italy",          "IT",                 "la Repubblica",            "No",    "Italian",
  "Portugal",          "PT",                           "RTP",           "Yes", "Portuguese",
  "Romania",          "RO",                           "TVR",           "Yes",    "Romanian",
  "Spain",          "ES",                          "RTVE",           "Yes",  "Spanish",
  "Switzerland",          "CH",              "SWI swissinfo.ch",            "No",    "English",
  "Sweden",          "SE",                           "SVT",           "Yes",     "Swedish",
  "Slovakia",          "SK",                      "Nový Čas",            "No",  "Slovak",
  "Slovenia",          "SI",                 "RTV Slovenija",           "Yes",  "Slovene"
)

metadata_df <- dat %>% 
  count(country) %>% 
  rename("Number of articles" = n) %>% 
  merge(metadata_df, by.x = "country", by.y = "Country code")

metadata_df <- dat %>% 
  group_by(country) %>% 
  mutate("Starting date" = as.character(ymd(min(date))), 
         "End date" = as.character(ymd(max(date)))) %>% 
  select(country, "Starting date", "End date") %>% 
  unique() %>% 
  merge(metadata_df, by = "country") %>% 
  rename("Country code" = country) %>% 
  arrange(Country)

select(metadata_df, 'Country', "Media outlet", "State-financed", "Starting date", "End date", "Number of articles") %>% 
  arrange("Country") %>% 
  knitr::kable(caption = "Description of the downloaded data from 31 country's media outlets.",
               align = c('l', rep('c', 5)))
```

| Country          |         Media outlet          | State-financed | Starting date |  End date  | Number of articles |
| :--------------- | :---------------------------: | :------------: | :-----------: | :--------: | :----------------: |
| Austria          |          Die Presse           |       No       |  2020-01-09   | 2021-01-31 |        6579        |
| Belgium (Dutch)  |              VRT              |      Yes       |  2020-02-04   | 2021-01-31 |       10549        |
| Belgium (French) |             RTBF              |      Yes       |  2020-02-04   | 2021-01-31 |       10549        |
| Bulgaria         | Bulgarian National Television |      Yes       |  2020-03-07   | 2021-01-31 |        3188        |
| Croatia          |         Večernji list         |       No       |  2020-01-28   | 2021-01-31 |        8236        |
| Cyprus           |          Cyprus Mail          |       No       |  2020-01-20   | 2021-01-31 |        3375        |
| Czech Republic   |        Česká televize         |      Yes       |  2020-03-05   | 2021-01-31 |        2997        |
| Denmark          |           Politiken           |       No       |  2020-02-11   | 2021-01-31 |        2593        |
| Estonia          |              ERR              |      Yes       |  2020-01-14   | 2021-01-31 |        3926        |
| Finland          |           Yle News            |      Yes       |  2020-03-19   | 2021-01-31 |        9505        |
| France           |           France 24           |      Yes       |  2020-01-06   | 2021-01-31 |        3001        |
| Germany          |          DER SPIEGEL          |       No       |  2020-02-02   | 2021-01-31 |        8224        |
| Greece           |              ERT              |      Yes       |  2020-02-18   | 2021-01-31 |        1283        |
| Hungary          |           hirado.hu           |      Yes       |  2020-01-09   | 2021-01-31 |        9063        |
| Iceland          |              RÚV              |      Yes       |  2020-02-01   | 2021-01-29 |        6366        |
| Italy            |         la Repubblica         |       No       |  2020-01-11   | 2021-01-31 |       30686        |
| Latvia           |              LSM              |      Yes       |  2020-02-27   | 2021-01-30 |        2670        |
| Lithuania        |        LRT televizija         |      Yes       |  2020-01-20   | 2021-01-30 |       10817        |
| Luxembourg       |          L’essentiel          |       No       |  2020-01-09   | 2021-01-31 |        1488        |
| Malta            |              TVM              |      Yes       |  2020-03-13   | 2021-01-31 |        2504        |
| Netherlands      |              NOS              |       No       |  2020-01-09   | 2021-01-31 |        4858        |
| Norway           |              NRK              |      Yes       |  2020-01-06   | 2021-01-31 |        2862        |
| Poland           |              TVP              |      Yes       |  2020-01-14   | 2021-01-31 |        8660        |
| Portugal         |              RTP              |      Yes       |  2020-03-10   | 2021-01-31 |       24497        |
| Romania          |              TVR              |      Yes       |  2020-02-22   | 2021-01-31 |        7646        |
| Slovakia         |           Nový Čas            |       No       |  2020-02-06   | 2021-01-31 |       14800        |
| Slovenia         |         RTV Slovenija         |      Yes       |  2020-01-09   | 2021-01-31 |        2494        |
| Spain            |             RTVE              |      Yes       |  2020-01-16   | 2021-01-31 |        7182        |
| Sweden           |              SVT              |      Yes       |  2020-01-22   | 2021-01-31 |        4351        |
| Switzerland      |       SWI swissinfo.ch        |       No       |  2020-01-20   | 2021-01-31 |        1008        |
| United Kingdom   |         The Guardian          |      Nem       |  2020-01-18   | 2021-01-27 |        3716        |

Description of the downloaded data from 31 country’s media outlets.

## Google translate

``` r
# Automatic translation =================================================================

Germany_rawtext %<>% 
  select(text) %>% 
  unnest_tokens(words, text)

st_de <- c(stopwords::stopwords('german'), "ab", "dass", "mehr", "worden", "wurde", "wurden", "sei") %>% 
  {ifelse(str_starts(., "új"), NA, .)} %>% 
  na.omit()

ggpubr::ggarrange(
  Germany_rawtext %>% 
    filter(!str_detect(words, '\\d')) %>% 
    anti_join(data.frame(words = st_de)) %>% 
    count(words, sort = T) %>% 
    arrange(desc(n)) %>% 
    head(29) %>% 
    mutate(
      words = fct_reorder(words, n)
    ) %>% 
    ggplot() +
    aes(n, words) + 
    geom_vline(xintercept = 0) +
    geom_col(color = 'black', fill = "gray70") +
    labs(title = 'Original text', x = 'Word count', y = NULL),
  dat_words_monthly %>% 
    filter(country == 'DE') %>% 
    group_by(country, words) %>% 
    summarise(n = sum(n)) %>% 
    ungroup() %>% 
    filter(!str_detect(words, '\\d')) %>% 
    anti_join(data.frame(words = c(stopwords::stopwords(), "also", "can"))) %>% 
    arrange(desc(n)) %>%
    left_join(modified_bing, 
              by=c("words"="word")) %>% 
    head(29) %>% 
    mutate(
      value = case_when(
        value < 0 ~ "Negative",
        value > 0 ~ "Positive", 
        T ~ "No sentiment"
      ),
      words = fct_reorder(words, n)
    ) %>% 
    ggplot() +
    aes(n, words, fill = value) + 
    geom_vline(xintercept = 0) +
    geom_col(color = "black") +
    labs(title = 'After translation', x = 'Word count', y = NULL, 
         fill = "Word sentiment") +
    scale_fill_manual(values = c('red4', 'gray70', 'green')) + 
    theme(
      legend.position = 'bottom',
      legend.direction = 'horizontal'
    ), common.legend = T
)
```

<div class="figure" style="text-align: center">

<img src="CoronaSentiment_files/figure-gfm/unnamed-chunk-3-1.png" alt="Leggyakrabban előforduló szavak a magyar nyelvű cikkekben a fordítást megelőzően és azt követően."  />

<p class="caption">

Leggyakrabban előforduló szavak a magyar nyelvű cikkekben a fordítást
megelőzően és azt követően.

</p>

</div>

``` r
dat_words_monthly %>% 
  group_by(date, words) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  group_modify(~ mutate(.x, n = n / sum(n))) %>% 
  anti_join(data.frame(words = c(stopwords::stopwords(), "also", "can"))) %>% 
  arrange(desc(n)) %>%
  group_modify(~ head(.x, 10)) %>%
  mutate(words = reorder_within(words, n, date)) %>%
  ggplot(aes(n, words)) + 
  geom_col(color = "black", fill = "cyan4") + 
  facet_wrap(~ date, scales = "free_y", labeller = as_labeller(function(x) str_sub(as.character(x), end = 7)))  +
  scale_y_reordered() + 
  labs(x = "Word frequency ratio", y = NULL)
```

<div class="figure" style="text-align: center">

<img src="CoronaSentiment_files/figure-gfm/unnamed-chunk-4-1.png" alt="A teljes korpusz leggyakoribb szavai, havonta"  />

<p class="caption">

A teljes korpusz leggyakoribb szavai, havonta

</p>

</div>

## The pandemic

``` r
library(ggforce)
library(glue)
library(ggtext)

dat_covid %>% 
  filter(code %in% unique(dat_sentiment_daily$country) &
           date < lubridate::ymd("2021-02-01") &
           date > lubridate::ymd("2019-12-31")
  ) %>% 
  group_by(date) %>% 
  summarise(new_cases = sum(new_cases, na.rm = T)) %>% 
  ungroup() %>% 
  merge(
    tibble(date = dmy(c("7th January 2020", "24th January 2020", "30th January 2020", 
                        "14th February 2020", "16th March 2020", "26th June 2020", 
                        "22nd September 2020", "23rd November 2020",
                        "21st December 2020")),
           text = c("The novel coronavirus is identified in Wuhan, China", 
                    "First confirmed European case in Bordeaux",
                    "The World Health Organization declares the outbreak a Public Health Emergency of International Concern",
                    "First confirmed European death in France",
                    "Many European countries deploy restrictions and declare state of emergency",
                    "Start of reopening in European nations",
                    "Owing to the easing during the summer, the number of new cases exceeds the first wave",
                    "Publishing of the third phase tests of vaccines Pfizer and AstraZeneca",
                    "The Pfizer vaccine is officially accepted by the European Medicines Agency")
    ), all = T
  ) %>% 
  tibble() %>% 
  merge(
    dat_sentiment_daily %>% 
      group_by(date) %>% 
      summarise(n_sentiment = sum(n)) %>% 
      ungroup(),
    all = T
  ) %>% 
  merge(
    tibble(date = seq.Date(from = lubridate::ymd('2019/12/31'), 
                           to = lubridate::ymd('2021/03/01'), by = "days")) %>% 
      mutate(t = row_number()), all.x = T
  ) %>% 
  tibble() %>% 
  mutate(
    new_cases = ifelse(is.na(new_cases), 0, new_cases),
    n_sentiment = ifelse(is.na(n_sentiment), 0, n_sentiment),
    new_cases = zoo::rollmean(new_cases, 7, na.pad=TRUE),
    n_sentiment = zoo::rollmean(n_sentiment, 7, na.pad=TRUE),
    n_sentiment = ifelse(is.na(n_sentiment), 0, n_sentiment),
    new_cases = ifelse(is.na(new_cases), 0, new_cases),
    date2 = ymd(ifelse(is.na(text), NA, as.character(date)))
  ) %>% 
  {
    
    ggplot(., aes(x =date, y = new_cases)) +
      geom_hline(yintercept = 0) +
      geom_line(color = "red") +
      geom_line(aes(date, n_sentiment*0.1), color = "blue") + 
      scale_y_continuous(
        name = "Number of daily new cases",
        sec.axis = sec_axis(~./0.1, name="Number of words with sentiment")
      ) + 
      geom_mark_circle(data = filter(., !is.na(text)), aes(x=date, y = n_sentiment, description = glue('"{text}"'),
                                                           label = glue("{date}:"), group = date), color = NA, 
                       expand = unit(2, "mm"), label.family = c("Oswald", "Poppins"), 
                       label.fontsize = 6,
                       label.buffer = unit(5, "mm"), con.size = 0.2) +
      labs(x = NULL, color = NULL) + 
      theme(
        legend.position = 'bottom'
      )
  }
```

<div class="figure" style="text-align: center">

<img src="CoronaSentiment_files/figure-gfm/timeline_plotting-1.png" alt="Az új esetek száma és a szentimentet tartalmazó szavak száma naponta"  />

<p class="caption">

Az új esetek száma és a szentimentet tartalmazó szavak száma naponta

</p>

</div>

``` r
#TODO Color error javítása
```

# Text analysis

## Topic model

``` r
dat_docmatrix<-readRDS(str_c(WD, "/data/topic_models/docmatrix.RDS"))
 
topic_models <- tibble(n_topic = c(2:16)) %>%
  mutate(
    file_name = str_c(WD, '/data/topic_models/topic_model', n_topic, '.RData'),
    # each RData contains one LDA model named as mod
    model = map(file_name, function(x) {load(x); return(mod)}),
    loglike = map_dbl(model, topicmodels::logLik),
    perplexity = map_dbl(model, topicmodels::perplexity),
    semantic_coh = map_dbl(model, function(x){sum(topicdoc::topic_coherence(x, dat_docmatrix))})
  )

topic_models %>% 
  select(-file_name,-model) %>% 
  pivot_longer(-1) %>% 
  ggplot(aes(n_topic, value)) + 
  geom_line() + 
  geom_point() +
  labs(x = 'Number of topics', y = 'Loglikelihood')+
  scale_x_continuous(breaks=seq(1, 16, 1)) +
  facet_wrap(~name, scale = "free_y", ncol = 2)
```

<div class="figure" style="text-align: center">

<img src="CoronaSentiment_files/figure-gfm/unnamed-chunk-5-1.png" alt="Finding the optimal number of topics for modelling"  />

<p class="caption">

Finding the optimal number of topics for modelling

</p>

</div>

``` r
mod_topic <- topic_models[["model"]][[11]]

#saveRDS(mod_topic, file = "data/final_topic_model.RDS")

tidy(mod_topic, matrix = "beta") %>%
  anti_join(rename(stop_words, term = word)) %>% 
  group_by(topic) %>%
  top_n(40, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term)) +
  geom_vline(xintercept = 0) +
  geom_col(show.legend = FALSE, color = 'black', fill = 'cyan4') +
  facet_wrap(~ topic, scales = "free_y", ncol = 3, labeller = as_labeller(
    function(x) paste('Topic', x)
  )) +
  scale_y_reordered() +
  labs(x = expression(beta), y = NULL)
```

<div class="figure" style="text-align: center">

<img src="CoronaSentiment_files/figure-gfm/unnamed-chunk-6-1.png" alt="Leggyakoribb szavak topikonként"  />

<p class="caption">

Leggyakoribb szavak topikonként

</p>

</div>

``` r
#TODO labels as topic names


tidy(mod_topic, matrix = "beta") %>% # unreported
  anti_join(rename(stop_words, term = word)) %>% 
  group_by(topic) %>%
  top_n(40, beta) %>%
  group_modify(.f = ~ tibble(terms = str_c(.x$term, collapse = ", "))) %>% 
  ungroup() %>% 
  kable(caption= "Most frequent words by topic", align = c("c", "l"))
```

| topic | terms                                                                                                                                                                                                                                                                                                                                                                   |
| :---: | :---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
|   1   | coronavirus, virus, 2020, corona, china, percent, industry, march, crisis, chinese, production, quarter, time, world, countries, billion, april, 2019, companies, passengers, million, economic, price, pandemic, sales, flights, demand, prices, increase, air, economy, compared, market, global, due, wuhan, company, decline, international, increased              |
|   2   | coronavirus, 2020, corona, 1, events, canceled, march, sports, positive, city, de, football, time, world, games, postponed, athletes, cup, national, fans, sport, team, start, game, players, announced, olympic, due, league, match, clubs, season, matches, training, teams, club, play, event, played, player                                                        |
|   3   | people, nursing, virus, homes, corona, care, home, health, crisis, measures, infection, infected, norwegian, time, day, employees, life, staff, doctors, norway, question, municipality, contact, lot, risk, week, situation, elderly, days, control, society, difficult, director, completely, stay, moment, weeks, believes, understand, means                        |
|   4   | people, coronavirus, county, virus, infections, 19, covid, hospital, patients, care, yesterday, home, health, infection, positive, total, infected, deaths, isolation, 2, 24, intensive, hours, day, center, confirmed, disease, employees, tests, died, tested, medical, staff, hospitalized, symptoms, hospitals, person, reported, days, diagnosed                   |
|   5   | coronavirus, emergency, government, 19, covid, european, minister, health, crisis, measures, epidemic, public, czech, time, euros, billion, support, law, employees, social, companies, tax, union, eu, million, budget, economic, commission, pandemic, ministry, prime, plan, economy, workers, money, due, situation, sector, added, financial                       |
|   6   | people, coronavirus, virus, government, infections, country, test, minister, home, health, measures, friday, police, infection, spread, infected, public, travel, countries, day, monday, border, restaurants, italy, ministry, closed, quarantine, prime, citizens, announced, allowed, foreign, risk, restrictions, due, situation, rules, authorities, days, ban     |
|   7   | people, coronavirus, vaccine, virus, government, vaccines, infections, 19, covid, vaccination, country, european, minister, health, epidemic, spread, infected, deaths, vaccinated, time, world, doses, countries, day, confirmed, disease, died, million, united, population, pandemic, increase, announced, week, authorities, reported, days, british, france, weeks |
|   8   | people, coronavirus, virus, news, corona, children, home, friends, city, time, world, day, media, social, family, left, pandemic, life, mother, found, lot, house, days, church, father, woman, live, lives, told, feel, night, video, started, stay, weeks, morning, son, times, ago, wife                                                                             |
|   9   | people, coronavirus, masks, virus, drug, 19, covid, patients, test, blood, health, infection, spread, infected, institute, 2, immune, world, testing, disease, tests, sars, information, research, medical, university, symptoms, system, researchers, data, treatment, antibodies, risk, study, clinical, cov, professor, results, diseases, scientists                |
|  10   | people, coronavirus, masks, corona, children, school, home, public, city, students, parents, time, day, mask, monday, online, schools, customers, information, service, distance, education, staff, services, university, closed, learning, student, contact, restrictions, due, transport, finland, week, teachers, situation, wear, safety, primary, classes          |
|  11   | people, coronavirus, government, food, 19, covid, home, cent, health, crisis, public, time, support, social, distancing, swiss, it’s, uk, pandemic, staff, services, switzerland, link, ireland, workers, restrictions, businesses, week, don’t, we’re, local, business, lockdown, irish, months, told, including, chief, weeks, government’s                           |
|  12   | people, coronavirus, virus, government, news, corona, china, country, minister, security, health, crisis, public, rights, trump, time, world, media, law, social, election, united, pandemic, biden, vote, president, party, elections, political, house, white, court, war, campaign, called, donald, american, democratic, fight, leader                              |

Most frequent words by topic

``` r
dat_topics %>% 
  mutate(date = ym(str_sub(as.character(date), end = -4))) %>% 
  group_by(date, country) %>% 
  summarise_all(.funs = function(x)  mean(x, na.rm = T)) %>% 
  ungroup() %>% 
  select(date, starts_with('topic')) %>% 
  group_by(date) %>% 
  summarise_all(.funs = function(x)  mean(x, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_longer(-1) %>% 
  mutate(name = factor(str_remove_all(name, 'topic_'), levels = as.character(1:12),
                       ordered = T)) %>% 
  ggplot() +
  aes(date, value, fill = name) +
  geom_col(color = 'black') + 
  labs(x = NULL, y = 'Relative frequency', fill = 'Topic') + 
  scale_y_continuous(labels = scales::percent) + 
  theme_minimal() +
  theme(
    legend.position = 'right',
    legend.direction = 'vertical'
  )
```

<div class="figure" style="text-align: center">

<img src="CoronaSentiment_files/figure-gfm/unnamed-chunk-7-1.png" alt="Topikok relatív megoszlásának időbeni dinamikája"  />

<p class="caption">

Topikok relatív megoszlásának időbeni dinamikája

</p>

</div>

``` r
topic_descript_df <- tibble( 
  topic = c(
    "Economy and travel",
    "Sport", 
    "Hospitals", 
    "Statistics", 
    "Governmental aids", 
    "Restrictions",
    "Vaccination",
    "Mental Health", 
    "Researches", 
    "Schools",
    "Work",
    "Politics"
  ),
  n = dat_topics %>% 
    select(country, starts_with('topic')) %>% 
    group_by(country) %>% 
    summarise_all(.funs = function(x) mean(x, na.rm = T)) %>% 
    ungroup() %>% 
    select(-country) %>% 
    apply(2, mean),
  date = dat_topics %>% 
    select(country, date, starts_with('topic')) %>% 
    mutate(date = ym(str_sub(as.character(date), end = -4))) %>% 
    group_by(country, date) %>% 
    summarise_all(.funs = function(x) mean(x, na.rm = T)) %>% 
    ungroup() %>% 
    select(-country) %>% 
    group_by(date) %>% 
    summarise_all(.funs = function(x) mean(x, na.rm = T)) %>% 
    {
      apply(.[2:13], 2, function(x) {
        as.character(.$date)[which.max(x)]
      })
    } %>% 
    ymd(),
  sentiment = dat_sentiment %>% 
    select(country, sent_mean, sent_n, top_topic) %>% 
    na.omit() %>% 
    group_by(country, top_topic) %>% 
    summarise(sentiment = weighted.mean(x = sent_mean,
                                        w = sent_n,
    )) %>% 
    ungroup() %>% 
    group_by(top_topic) %>% 
    summarise(sentiment = mean(sentiment)) %>% 
    #arrange(desc(top_topic)) %>% 
    .$sentiment
) %>% 
  arrange(desc(n))

ggplot(data = topic_descript_df) +
  geom_hline(aes(yintercept = 0), linetype = 2, color = 'grey20') +
  geom_point(aes(x = date, y = (sentiment-mean(sentiment))*n, fill = sentiment, size = n)) +
  scale_size(range = c(10, 20), breaks = c(.1), 
             labels = function(x) scales::percent(x, accuracy = 1, decimal.mark = ',')) + # TODO
  scale_fill_gradient(low = 'cyan4', high = 'grey90', guide = guide_colorsteps()) +
  geom_text(mapping = aes(x = date, y = (sentiment-mean(sentiment))*n, label = topic), 
            show.legend = F, size = 3) + 
  geom_text(mapping = aes(x = ymd('2020-03-01'), y = -.00008), 
            color = 'grey20', size = 3,
            label = "Average of topics' sentiments") +
  labs(x = "Most typical month of topics", y = 'Sentiment influence',
       fill = 'Average sentiment', size = 'Relative frequency (size)') +
  theme_minimal() + 
  theme(
    legend.position = 'bottom',
    legend.box = "vertical",
    legend.key.width = unit(1, 'cm')
  )
```

<div class="figure" style="text-align: center">

<img src="CoronaSentiment_files/figure-gfm/unnamed-chunk-8-1.png" alt="Topikok jellemzése szentiment, dátum és gyakoriság szerint"  />

<p class="caption">

Topikok jellemzése szentiment, dátum és gyakoriság szerint

</p>

</div>

## Sentiment

``` r
bing_comparison %>% 
  filter(sentiment != my_sentiment_2) %>% 
  arrange(desc(n)) %>% 
  mutate(my_sentiment = ifelse(my_sentiment_2 == "NA", 0, my_sentiment_2)) %>% 
  select(word, n, sentiment, my_sentiment = my_sentiment_2) %>% 
  head(23) %>% 
  kable(caption = "Most frequent words with changed sentiments because of corpus-specific context", col.names = c("Word", "Frequency", "Old sentiment", "New sentiment"))
```

| Word        | Frequency | Old sentiment | New sentiment |
| :---------- | --------: | ------------: | ------------: |
| positive    |     80778 |             1 |           \-1 |
| negative    |     25899 |           \-1 |             1 |
| patient     |     22501 |             1 |           \-1 |
| positives   |     10751 |             1 |           \-1 |
| tough       |      3382 |             1 |           \-1 |
| defeat      |      3267 |             1 |           \-1 |
| tougher     |      1661 |             1 |           \-1 |
| boom        |      1388 |             1 |           \-1 |
| cheap       |      1071 |           \-1 |             1 |
| elimination |       900 |           \-1 |             1 |
| funny       |       676 |           \-1 |             1 |
| toughest    |       444 |             1 |           \-1 |
| negativity  |       321 |           \-1 |             1 |
| negatives   |       317 |           \-1 |             1 |
| frugal      |       307 |             1 |           \-1 |
| sharpest    |       246 |             1 |           \-1 |

Most frequent words with changed sentiments because of corpus-specific
context

``` r
library(reshape2)

dat_words_monthly %>% 
  group_by(country, words) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  filter(!str_detect(words, '\\d')) %>% 
  anti_join(data.frame(words = c(stopwords::stopwords(), "also", "can"))) %>% 
  arrange(desc(n)) %>%
  left_join(modified_bing, 
            by=c("words"="word")) %>% 
  mutate(
    sentiment = ifelse(value > 0, "Positive", "Negative")
  ) %>% 
  na.omit() %>% 
  arrange(desc(n)) %>% 
  group_by(sentiment) %>% 
  group_modify(~ head(.x, 50)) %>% 
  ungroup() %>% 
  acast(words ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("cyan4", "red4"),
                   max.words = 100)
```

<div class="figure" style="text-align: center">

<img src="CoronaSentiment_files/figure-gfm/unnamed-chunk-10-1.png" alt="Leggyakrabban előforduló pozitív és negatív szentimenttel rendelkező szavak"  />

<p class="caption">

Leggyakrabban előforduló pozitív és negatív szentimenttel rendelkező
szavak

</p>

</div>

``` r
library(ggraph)
library(igraph)

set.seed(2021)

f_colorise <- function(x) {
  pos <- modified_bing %>% 
    filter(value == 1) %>% 
    pull(word)
  neg <- modified_bing %>% 
    filter(value == -1) %>% 
    pull(word)
  case_when(
    x %in% pos ~ 'positive',
    x %in% neg ~ 'negative',
    T ~ 'neutral'
  )
}

dat %>% 
  {.[sample(nrow(.)), ]} %>% 
  group_by(country) %>% 
  group_modify(~head(.x, 100)) %>% # TODO increase
  ungroup() %>% 
  mutate(r = row_number()) %>% 
  select(r, text) %>% 
  unnest_tokens(words, text) %>% 
  anti_join(data.frame(words = c(stopwords::stopwords(), "also", "can"))) %>% 
  count(r, words, sort = T) %>% 
  group_by(words) %>%
  filter(n() >= 20) %>%
  widyr::pairwise_cor(words, r, sort = TRUE) %>% 
  filter(item1 %in% modified_bing$word) %>% 
  head(100) %>% 
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(aes(color = f_colorise(name)), size = 5, shape = 16) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() + 
  labs(color = 'Sentiment')
```

    ## Error: Unknown colour name: negative

<div class="figure" style="text-align: center">

<img src="CoronaSentiment_files/figure-gfm/unnamed-chunk-11-1.png" alt="Szentimenttel bíró szavakkal korreláló szavak hálója"  />

<p class="caption">

Szentimenttel bíró szavakkal korreláló szavak hálója

</p>

</div>

``` r
dat_sentiment_daily %>% 
  group_by(date) %>% 
  summarise_at(c('n_total', 'n'), .funs = function(x) sum(x, na.rm = T)) %>% 
  ggplot(aes(date, n/n_total)) +
  geom_line(color = '#595959', size = .8) +
  geom_line(aes(date, zoo::rollmean(n/n_total, 7, na.pad=TRUE), 
                color = 'Rolling 7-day average'), size = 1.3) +
  scale_color_manual(values = c('#E3120B')) + 
  labs(x = NULL, y = 'Ratio of words with sentiment', color = NULL)
```

    ## Error: Can't subset columns that don't exist.
    ## x Column `n_total` doesn't exist.

``` r
# Explore the data ----------------------------------------------------------------------

dat_sentiment_daily %>% 
  mutate(code = country) %>% 
  ggplot(aes(date, sent_mean)) +
  geom_hline(yintercept = 0, color = "grey20") +
  geom_line(size = .3, color = 'grey50') +
  geom_smooth(size = 1.5, se = F) +
  facet_geo(~ code, grid = mygrid, label = 'name') +
  scale_x_date(limits = c(min(dat_sentiment_daily$date), max(dat_sentiment_daily$date)),
               breaks = c(min(dat_sentiment_daily$date), max(dat_sentiment_daily$date))) +
  labs(y = "Sentiment", x = NULL)
```

<div class="figure" style="text-align: center">

<img src="CoronaSentiment_files/figure-gfm/unnamed-chunk-13-1.png" alt="A szentiment alakulása országonként" angle=90 />

<p class="caption">

A szentiment alakulása országonként

</p>

</div>

## TF-IDF

``` r
dat_words_monthly %>% 
  filter(words != "feff") %>% 
  mutate(month = ifelse(year(date)>2020,month(date)+12, month(date))) %>% 
  group_by(month, words) %>% 
  summarise(monthly_n = sum(n)) %>% 
  bind_tf_idf(words, month, monthly_n) %>%
  arrange(desc(tf_idf)) %>% 
  ungroup() %>% 
  filter(!(words %in% c(stopwords("english"), "also", "one", "will", "although"))) %>% 
  filter(!str_detect(words, "[0-9,]+")) %>% 
  filter(str_detect(words, "^[A-Za-z]+$")) %>% 
  filter(!str_detect(words, "serif")) %>% 
  filter(!(words %in% (dat_words_monthly %>% # filter words from ONE country
                         group_by(words) %>% 
                         mutate(nr_country = n_distinct(country))  %>% 
                         ungroup() %>% 
                         filter(nr_country < 3) %>% 
                         pull(words)))) %>% 
  filter(tf >0.00005) %>% 
  group_by(month) %>%
  slice_max(tf_idf, n = 3) %>%
  ungroup() %>%
  mutate(month = as.factor(month),
         words = reorder_within(words, tf_idf, month)
  ) %>% 
  ggplot(aes(tf_idf, y = words, fill = month)) +
  geom_vline(xintercept = 0) +
  geom_col(show.legend = F, color = 'black') +
  scale_y_reordered() +
  scale_fill_hue(h = c(200, 300)) +
  scale_x_continuous(breaks = c(0, 0.00005, 0.0001), limits = c(0, 0.0001), 
                     expand = c(0, 0)) +
  facet_wrap(~month, ncol = 3, scales = "free_y", labeller = as_labeller(function(x) {
    c(paste0('2020-', 1:12), '2021-01')[as.numeric(x)]
  })
  ) +
  labs(x = "TF-IDF", y = NULL)
```

<div class="figure" style="text-align: center">

<img src="CoronaSentiment_files/figure-gfm/tf_idf_per_month-1.png" alt="A cikkekben havonta leginkább jellemző szavak, TF-IDF értékek alapján"  />

<p class="caption">

A cikkekben havonta leginkább jellemző szavak, TF-IDF értékek alapján

</p>

</div>

# Econometrics

## Descriptive statistics

``` r
dat_plm <- dat_eco_sent %>% 
  filter(indic == "BS-ESI-I") %>% 
  select(date = time, code = geo, eco = values) %>% 
  merge(mutate(dat_sentiment_monthly, code = country), all = T) %>% 
  merge(dat_unemployment, all = T) %>% 
  merge(dat_covid_monthly, all = T) %>% 
  mutate(
    t = lubridate::interval(lubridate::ymd('2020-01-01'), date),
    t = lubridate::as.period(t) %/% months(1),
    cases = ifelse(is.na(cases), 0, cases),
    death = ifelse(is.na(death), 0, death),
    new_cases = ifelse(is.na(new_cases), 0, new_cases),
    new_deaths = ifelse(is.na(new_deaths), 0, new_deaths)
  ) %>% 
  select(-country, -n) %>% 
  pivot_longer(-c(1:2)) %>% 
  {
    rbind(.,
          mutate(., 
                 name = paste0(name, '_l'),
                 date = date %m+% months(1)
          )
    )
  } %>%
  pivot_wider(names_from = name, values_from = value) %>% 
  mutate(
    cases_l = ifelse(is.na(cases_l), 0, cases_l),
    death_l = ifelse(is.na(death_l), 0, death_l)
  ) %>% 
  mutate(t_2 = t*t) %>% 
  mutate(
    season = case_when(
      date < ymd('2020-03-01') ~ 'Winter 2019/2020',
      date < ymd('2020-06-01') ~ 'Spring 2020',
      date < ymd('2020-09-01') ~ 'Summer 2020',
      date < ymd('2020-12-01') ~ 'Autumn 2020',
      T ~ 'Winter 2020/2021'
    ),
    season = factor(season, levels = c('Winter 2019/2020', 'Spring 2020', 'Summer 2020', 'Autumn 2020',
                                       'Winter 2020/2021'))
  ) %>% 
  filter(!is.na(sent_mean)) %>% 
  select(code, date, everything())
```

``` r
# Regression tree -----------------------------------------------------------------------
dat_plm %>% 
  select(sent_mean, eco, unemployment, cases, death, new_cases, new_deaths, t) %>% 
  set_names('Sentiment', 'ESI', 'Unemployment', 'Number of cases (per 1000)', 'Number of deaths (per 1000)',
            'Number of cases', 'Number of deaths', 't') %>% 
  rpart::rpart(formula = Sentiment ~.,
               cp = .012) %>% 
  rattle::fancyRpartPlot(palettes = 'PuRd', sub = NULL)
```

<div class="figure" style="text-align: center">

<img src="CoronaSentiment_files/figure-gfm/unnamed-chunk-15-1.png" alt="Regressziós fa"  />

<p class="caption">

Regressziós fa

</p>

</div>

``` r
dat_plm %>% 
  mutate(
    date = str_sub(as.character(date), end = -4)
  ) %>% 
  ggplot(aes(eco, sent_mean, fill = season, label = paste(date, code))) + 
  geom_point(size = 2) +
  ggrepel::geom_text_repel(max.overlaps = 5) +
  theme_bw() + 
  theme(legend.position = 'bottom') +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = 'Economic Sentiment Index', y = 'Media sentiment', fill = NULL)
```

<div class="figure" style="text-align: center">

<img src="CoronaSentiment_files/figure-gfm/unnamed-chunk-16-1.png" alt="A hírekben megjelenő szentiment és gazdaság érzékelési index közötti kapcsolat"  />

<p class="caption">

A hírekben megjelenő szentiment és gazdaság érzékelési index közötti
kapcsolat

</p>

</div>

## Panel models

``` r
panel_models <- tibble(
  formula = c(
    "sent_mean ~ death + eco",
    "sent_mean ~ death + eco + season + death:t"
  )
) %>% 
  mutate(
    pooling = map(formula, ~ plm::plm(data = dat_plm, formula = eval(.), model = "pooling")),
    within = map(formula, ~ plm::plm(data = dat_plm, formula = eval(.), model = "within")),
    # random model cannot be estimated
    pooltest_pvalue = pmap(list(pooling, within), plm::pooltest),
    pooltest_pvalue = map_dbl(pooltest_pvalue, "p.value"),
    r_within = map_dbl(within, plm::r.squared, dfcor = T)
  )

knitr::kable(select(panel_models, -pooling, -within))
```

| formula                                      | pooltest\_pvalue | r\_within |
| :------------------------------------------- | ---------------: | --------: |
| sent\_mean \~ death + eco                    |                0 | 0.2811711 |
| sent\_mean \~ death + eco + season + death:t |                0 | 0.6516074 |

``` r
pwalk(list(model = pull(panel_models, within), 
           title = c("Regressziós eredmények időszakra való kontrollálás nélkül", 
                     "Becsült regresszió paraméterek kontrollálva az időszakokra")), 
      function(model, title) {
        tidy(model) %>% 
          kable(caption = title) %>% 
          print()
      })
```

| term  |    estimate | std.error |   statistic | p.value |
| :---- | ----------: | --------: | ----------: | ------: |
| death |   0.2134282 | 0.0419071 |    5.092893 |   6e-07 |
| eco   | \-0.0055007 | 0.0004939 | \-11.136370 |   0e+00 |

Regressziós eredmények időszakra való kontrollálás nélkül

| term                   |    estimate | std.error |  statistic |   p.value |
| :--------------------- | ----------: | --------: | ---------: | --------: |
| death                  | \-0.2037525 | 0.1109831 | \-1.835889 | 0.0673349 |
| eco                    | \-0.0017471 | 0.0004733 | \-3.691289 | 0.0002637 |
| seasonSpring 2020      |   0.2576966 | 0.0158607 |  16.247521 | 0.0000000 |
| seasonSummer 2020      |   0.2259041 | 0.0149750 |  15.085391 | 0.0000000 |
| seasonAutumn 2020      |   0.2164940 | 0.0138197 |  15.665577 | 0.0000000 |
| seasonWinter 2020/2021 |   0.2402200 | 0.0177793 |  13.511185 | 0.0000000 |
| death:t                |   0.0295632 | 0.0114849 |   2.574100 | 0.0105158 |

Becsült regresszió paraméterek kontrollálva az időszakokra
