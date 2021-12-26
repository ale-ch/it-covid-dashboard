library(tidyverse)
library(lubridate)

##### carica dati ####
url <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv"

df <- read.csv(url(url)) %>% 
  select(-c(2, 12, 13, 16:24)) %>% 
  mutate(data = ymd_hms(data))

#### nuove variabili ####
variazione_tamponi <- vector("numeric", nrow(df))
variazione_tamponi[1] <- df[1, "tamponi"]
for(i in 2:nrow(df)) {
  variazione_tamponi[i] <- df[i, "tamponi"] - df[i-1, "tamponi"]
}

Rt <- vector("numeric", nrow(df))
Rt[1] <- NA
for(i in 2:nrow(df)) {
  Rt[i] <- df[i, "nuovi_positivi"] / df[i-1, "nuovi_positivi"]
}

#### sovrascrivi dataset ####
df <- df %>% 
  mutate(
    variazione_tamponi = variazione_tamponi,
    ratio_positivi_tamponi = (nuovi_positivi / variazione_tamponi),
    Rt = Rt)

rm(list = c("i", "Rt", "url", "variazione_tamponi"))

