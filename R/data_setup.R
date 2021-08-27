dat_covid <- readr::read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>% 
  transmute(code = countrycode::countrycode(iso_code, origin = 'iso3c', 
                                            destination = 'iso2c'),
            date, 
            cases = new_cases_per_million/1000,
            death = new_deaths_per_million/1000,
            new_cases, new_deaths
  )

dat_covid_monthly <- dat_covid %>% 
  mutate(
    date = lubridate::ym(paste0(lubridate::year(date), '-', lubridate::month(date)))
  ) %>% 
  group_by(date, code) %>% 
  summarise_all(.funs = function(x) sum(x, na.rm = T)) %>% 
  ungroup()

dat_eco_sent <- eurostat::get_eurostat('ei_bssi_m_r2')
# Economic sentiment indicator

dat_unemployment <- eurostat::get_eurostat("une_rt_m") %>% 
  # unemployment
  filter(age == "TOTAL", sex == "T", s_adj == "NSA", unit == "PC_ACT") %>% 
  select(date = time, code = geo, unemployment = values) 

mygrid <- data.frame(
  row = c(5, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 2, 5),
  col = c(7, 1, 3, 4, 7, 7, 5, 4, 2, 3, 7, 2, 3, 5, 4, 4, 7, 6, 2, 5, 3, 6, 4, 5, 2, 4, 7, 1, 6, 1, 3),
  code = c("BG", "IS", "NO", "SE", "EE", "LV", "FI", "DK", "UK", "NL", "LT", "BE", "LU", "PL", "CZ", "AT", "RO", "HU", "FR", "SK", "DE", "HR", "IT", "SI", "ES", "MT", "CY", "PT", "EL", "IE", "CH"),
  name = c("Bulgária", "Izland", "Norvégia", "Svédország", "Észtország", "Lettország", "Finnország", "Dánia", "Egyesült Királyság", "Hollandia", "Litvánia", "Belgium", "Luxemburg", "Lengyelország", "Csehország", "Ausztria", "Románia", "Magyarország", "Franciaország", "Szlovákia", "Németország", "Horvátország", "Olaszország", "Szlovénia", "Spanyolország", "Málta", "Ciprus", "Portugália", "Görögország", "Írország", "Svájc"),
  stringsAsFactors = FALSE
)
