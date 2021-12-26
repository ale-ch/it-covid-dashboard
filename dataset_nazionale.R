library(tidyverse)
library(lubridate)

##### carica dati ####
url <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv"

df <- read.csv(url(url)) %>% 
  select(-c(2, 12, 13, 16:24)) %>% 
  mutate(data = ymd_hms(data))

#### sovrascrivi dataset ####
df <- df %>% 
  mutate(
    nuovi_tamponi = c(tamponi[1], diff(tamponi, 1)),
    ratio_positivi_tamponi = (nuovi_positivi / nuovi_tamponi),
    Rt = lag(lead(nuovi_positivi)) / lag(nuovi_positivi),
    denominazione_regione = "Nazionale")

rm(list = url)
