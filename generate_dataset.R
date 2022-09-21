library(tidyverse)
library(lubridate)

##### load data ####
url1 <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv"
url2 <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"

#### national data ####
df <- read.csv(url(url1)) %>% 
  select(-c(2, 12, 13, 16:24, totale_casi,
            variazione_totale_positivi, totale_ospedalizzati,
            ricoverati_con_sintomi, totale_positivi)) %>% 
  mutate(data = ymd_hms(data),
         tests = c(tamponi[1], diff(tamponi, 1)),
         pos_tests_ratio = (nuovi_positivi / tests),
         discharged_healed = c(dimessi_guariti[1], diff(dimessi_guariti, 1)),
         Rt = lag(lead(nuovi_positivi)) / lag(nuovi_positivi),
         denominazione_regione = "National")

#### regional data ####
df_regioni <- read.csv(url(url2)) %>% 
  select(
    -c(codice_nuts_1, codice_nuts_2, note_casi, note_test, 
       note, casi_da_screening, casi_da_sospetto_diagnostico,
       tamponi_test_antigenico_rapido, tamponi_test_molecolare,
       totale_positivi_test_antigenico_rapido, 
       totale_positivi_test_molecolare, ingressi_terapia_intensiva,
       casi_testati, totale_casi,
       variazione_totale_positivi, totale_ospedalizzati,
       ricoverati_con_sintomi, totale_positivi)) %>% 
  mutate(data = ymd_hms(data)) %>% 
  group_by(denominazione_regione) %>% 
  mutate(
    tests = c(tamponi[1], diff(tamponi)),
    pos_tests_ratio = (nuovi_positivi / tests),
    discharged_healed = c(dimessi_guariti[1], diff(dimessi_guariti, 1)),
    Rt = lag(lead(nuovi_positivi)) / lag(nuovi_positivi)) %>% 
  ungroup()

#### join ####
shared_names <- names(df_regioni)[which(names(df_regioni) %in% names(df))]

covid_italy <- full_join(df, df_regioni, by = shared_names)

covid_italy <- covid_italy %>% 
  rename(
    date = data,
    region_name = denominazione_regione,
    positives = nuovi_positivi,
    intensive_care = terapia_intensiva,
    quarantined = isolamento_domiciliare,
    deceased = deceduti,
  ) %>% 
  select(-c(tamponi, stato, codice_regione, dimessi_guariti))
