library(tidyverse)
library(lubridate)

url <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"

df_regioni <- read.csv(url(url)) %>% 
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
    Rt = lag(lead(df$nuovi_positivi)) / lag(df$nuovi_positivi)
  ) %>% 
  ungroup() 




