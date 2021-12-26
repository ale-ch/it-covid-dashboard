covid_plot <- function(df = list(), last_n_days = double(), 
                       var = character(), variazione = boolean(),
                       percentuale = boolean(), regione = "Nazionale") {
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  
  #### stopping condition #####
  max_days <- round(max(df$data) - df$data[1], 0)
  
  if(last_n_days > max_days) {
    stop(paste0("Data is only available for the last ", 
                as.character(max_days), " days"))
  }
  
  #### plotting ####
  name <- var
  var <- unlist(unname(df[name]))
  
  df <- df %>% 
    mutate(var = var)
  
  # show variation: yes/no 
  if(variazione == TRUE) {
    df <- df %>% 
      group_by(denominazione_regione) %>% 
      mutate(var = c(var[1], diff(var))) %>% 
      ungroup()

    name <- paste0("variazione_", name)
  } 
  
  p <- df %>% 
    filter(data >= max(data) - days(last_n_days),
           denominazione_regione %in% regione) %>% 
    ggplot(aes(data, var, group = denominazione_regione, 
               color = denominazione_regione)) +
    geom_line() +
    ylab(name)
 
  # show percentages 
  if(percentuale == TRUE) {
    p <- p + scale_y_continuous(labels = scales::percent)
  }
  
  p
}


#### tests ####
#covid_plot(df = df_regioni, last_n_days = 14, var = "totale_positivi", 
#           variazione = T, percentuale = F, 
#           regione = c("Lombardia", "Veneto", "Sardegna"))
#
#covid_plot(df = df_regioni, last_n_days = 14, var = "ratio_positivi_tamponi", 
#           variazione = F, percentuale = T, 
#           regione = c("Lombardia", "Veneto", "Sardegna"))




