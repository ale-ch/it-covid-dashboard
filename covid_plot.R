covid_plot <- function(df = data.frame(), last_n_days = double(), 
                       var = character(), variazione = boolean(),
                       percentuale = boolean()) {
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
  var <- unlist(unname(df[var]))
  
  # show variation: yes/no 
  if(variazione == TRUE) {
    delta <- vector("numeric", nrow(df))
    delta[1] <- df[1, name]
    for(i in 2:nrow(df)) {
      delta[i] <- df[i, name] - df[i-1, name] 
    }
    var <- delta
    name <- paste0("variazione_", name)
  }
  
  # plot 
  p <- df %>% 
    mutate(var = var) %>% 
    filter(data >= max(data) - days(last_n_days)) %>% 
    ggplot(aes(data, var)) +
    geom_line() +
    ylab(name)
  
  if(percentuale == TRUE) {
    p <- p + scale_y_continuous(labels = scales::percent)
  }
  
  p
}





