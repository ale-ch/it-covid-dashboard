library(tidyverse)
library(lubridate)

##### carica dati ####
url1 <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv"
url2 <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"


#### dati nazionali ####
df <- read.csv(url(url1)) %>% 
  select(-c(2, 12, 13, 16:24)) %>% 
  mutate(data = ymd_hms(data),
         nuovi_tamponi = c(tamponi[1], diff(tamponi, 1)),
         ratio_positivi_tamponi = (nuovi_positivi / nuovi_tamponi),
         Rt = lag(lead(nuovi_positivi)) / lag(nuovi_positivi),
         denominazione_regione = "Nazionale") 

#### dati regionali ####
df_regioni <- read.csv(url(url2)) %>% 
  select(
    -c(codice_nuts_1, codice_nuts_2, note_casi, note_test, 
       note, casi_da_screening, casi_da_sospetto_diagnostico,
       tamponi_test_antigenico_rapido, tamponi_test_molecolare,
       totale_positivi_test_antigenico_rapido, 
       totale_positivi_test_molecolare, ingressi_terapia_intensiva,
       casi_testati)) %>% 
  mutate(data = ymd_hms(data)) %>% 
  group_by(denominazione_regione) %>% 
  mutate(
    nuovi_tamponi = c(tamponi[1], diff(tamponi)),
    ratio_positivi_tamponi = (nuovi_positivi / nuovi_tamponi),
    Rt = lag(lead(nuovi_positivi)) / lag(nuovi_positivi)) %>% 
  ungroup()

#### join ####
shared_names <- names(df_regioni)[which(names(df_regioni) %in% names(df))]

covid_italy <- full_join(df, df_regioni, by = shared_names)


# clean environment
rm(list = c("url1", "url2", "shared_names"))
