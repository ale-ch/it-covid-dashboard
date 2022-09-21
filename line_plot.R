line_plot <- function(data = list(), last_n_days = double(), 
                       var = character(), variation = boolean(),
                       percentage = boolean(), region = "National") {
  library(dplyr)
  library(plotly)
  library(lubridate)
  
  #### stopping condition #####
  max_days <- round(max(data$date) - data$date[1], 0)
  
  if(last_n_days > max_days) {
    stop(paste0("Data is only available for the last ", 
                as.character(max_days), " days"))
  }
  
  #### plotting ####
  name <- var
  var <- unlist(unname(data[name]))
  
  data <- data %>% 
    mutate(var = var)
  
  # show variation: yes/no 
  if(variation == TRUE) {
    data <- data %>% 
      group_by(region_name) %>% 
      mutate(var = c(var[1], diff(var))) %>% 
      ungroup()
    
    name <- paste0("variation_", name)
  } 
  
  p <- data %>% 
    filter(date >= max(date) - days(last_n_days),
           region_name %in% region) %>%
    plot_ly(type = "scatter", mode = "lines",
            color = ~region_name) %>% 
    add_trace(x = ~date, y = ~var) %>% 
    layout(showlegend = FALSE, 
           yaxis = list(title = name))
  
  # show percentages 
  if(percentage == TRUE) {
    p <- p %>% layout(
      yaxis = list(title = name, tickformat = '%')
      )
  }
  
  p
}
