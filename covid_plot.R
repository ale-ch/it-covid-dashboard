covid_plot <- function(df = list(), last_n_days = double(), 
                       var = character(), variation = boolean(),
                       percentage = boolean(), region = "National") {
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  
  #### stopping condition #####
  max_days <- round(max(df$date) - df$date[1], 0)
  
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
  if(variation == TRUE) {
    df <- df %>% 
      group_by(region_name) %>% 
      mutate(var = c(var[1], diff(var))) %>% 
      ungroup()

    name <- paste0("variation_", name)
  } 
  
  p <- df %>% 
    filter(date >= max(date) - days(last_n_days),
           region_name %in% region) %>% 
    ggplot(aes(date, var, group = region_name, 
               color = region_name)) +
    geom_line() +
    ylab(name)
 
  # show percentages 
  if(percentage == TRUE) {
    p <- p + scale_y_continuous(labels = scales::percent)
  }
  
  p
}
