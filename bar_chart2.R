library(plotly)

bar_chart2 <- function(data) {
  df <- data %>% 
    filter(region_name != "National", date == max(date))
  
  fig <- df %>% plot_ly()
  fig <- fig %>% add_trace(x = ~df$region_name, 
                           y = ~df$intensive_care, 
                           type = 'bar',
                           name = "Intensive care",
                           text = df$intensive_care, 
                           textposition = 'auto',
                           marker = list(color = 'red',
                                         line = list(color = 'rgb(8,48,107)', 
                                                     width = 1.5)))
  fig <- fig %>% add_trace(x = ~df$region_name, 
                           y = ~df$positives, 
                           type = 'bar',
                           name = "New positives",
                           text = df$positives, textposition = 'auto',
                           marker = list(color = 'orange',
                                         line = list(color = 'rgb(8,48,107)', 
                                                     width = 1.5)))
  fig <- fig %>% add_trace(x = ~df$region_name, 
                           y = ~df$discharged_healed, 
                           type = 'bar',
                           name = "Discharged/Healed",
                           text = df$discharged_healed, 
                           textposition = 'auto',
                           marker = list(color = 'green',
                                         line = list(color = 'rgb(8,48,107)', 
                                                     width = 1.5)))
  fig <- fig %>% layout(barmode = 'group',
                        xaxis = list(title = ""),
                        yaxis = list(title = ""))
  
  fig
}

