library(tidyverse)
library(lubridate)

##### carica dati ####
url1 <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv"
url2 <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"

#### dati nazionali ####
df <- read.csv(url(url1)) %>% 
  select(-c(2, 12, 13, 16:24, totale_casi,
            variazione_totale_positivi, totale_ospedalizzati,
            ricoverati_con_sintomi, totale_positivi, dimessi_guariti)) %>% 
  mutate(data = ymd_hms(data),
         tests = c(tamponi[1], diff(tamponi, 1)),
         pos_tests_ratio = (nuovi_positivi / tests),
         Rt = lag(lead(nuovi_positivi)) / lag(nuovi_positivi),
         denominazione_regione = "National")

#### dati regionali ####
df_regioni <- read.csv(url(url2)) %>% 
  select(
    -c(codice_nuts_1, codice_nuts_2, note_casi, note_test, 
       note, casi_da_screening, casi_da_sospetto_diagnostico,
       tamponi_test_antigenico_rapido, tamponi_test_molecolare,
       totale_positivi_test_antigenico_rapido, 
       totale_positivi_test_molecolare, ingressi_terapia_intensiva,
       casi_testati, totale_casi,
       variazione_totale_positivi, totale_ospedalizzati,
       ricoverati_con_sintomi, totale_positivi, dimessi_guariti)) %>% 
  mutate(data = ymd_hms(data)) %>% 
  group_by(denominazione_regione) %>% 
  mutate(
    tests = c(tamponi[1], diff(tamponi)),
    pos_tests_ratio = (nuovi_positivi / tests),
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
  select(-c(tamponi, stato, codice_regione, lat, long))



# 95% confidence interval for proportion of infections over the number of daily tests
p_hat <- covid_italy$pos_tests_ratio
n <- covid_italy$tests

bound_1 <- round(p_hat - qnorm(0.95) * sqrt(p_hat * (1 - p_hat) / n), 5)
bound_2 <- round(p_hat + qnorm(0.95) * sqrt(p_hat * (1 - p_hat) / n), 5)

ratio_confint_95 <- paste(as.character(bound_1), as.character(bound_2))

covid_italy <- covid_italy %>% 
  mutate(ratio_confint_95 = ratio_confint_95)


# clean environment
rm(list = c("url1", "url2", "shared_names", "df", 
            "df_regioni", "p_hat", "n", "bound_1", 
            "bound_2", "ratio_confint_95"))
