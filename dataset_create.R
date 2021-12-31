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

# 95% confidence interval for proportion of infections over the number of daily tests
p_hat <- covid_italy$ratio_positivi_tamponi
n <- covid_italy$nuovi_tamponi

marg_1 <- round(p_hat - qnorm(0.95) * sqrt(p_hat * (1 - p_hat) / n), 5)
marg_2 <- round(p_hat + qnorm(0.95) * sqrt(p_hat * (1 - p_hat) / n), 5)

ratio_confint_95 <- paste(as.character(marg_1), as.character(marg_2))

covid_italy <- covid_italy %>% 
  mutate(ratio_confint_95 = ratio_confint_95)


# clean environment
rm(list = c("url1", "url2", "shared_names", "df", "df_regioni", "p_hat", "n", "marg_1", "marg_2", "ratio_confint_95"))
