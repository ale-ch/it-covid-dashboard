library(tidyverse)
library(lubridate)

##### carica dati ####
url <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv"

df <- read.csv(url(url)) %>% 
  select(-c(2, 12, 13, 16:24)) %>% 
  mutate(data = ymd_hms(data))

#### nuove variabili ####

# numero di test svolti giornalmente
nuovi_tamponi <- diff(df$tamponi, 1)
nuovi_tamponi <- c(df$tamponi[1], nuovi_tamponi)

# tasso Rt giornaliero
Rt <- vector("numeric", nrow(df))
Rt[1] <- NA
for(i in 2:nrow(df)) {
  Rt[i] <- df[i, "nuovi_positivi"] / df[i-1, "nuovi_positivi"]
}

#### sovrascrivi dataset ####
df <- df %>% 
  mutate(
    nuovi_tamponi = nuovi_tamponi,
    ratio_positivi_tamponi = (nuovi_positivi / nuovi_tamponi),
    Rt = Rt)

rm(list = c("i", "Rt", "url", "nuovi_tamponi"))

