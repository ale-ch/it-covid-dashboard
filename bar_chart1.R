library(plotly)

bar_chart1 <- function(data) {
  df <- data %>% 
    filter(region_name != "National", date == max(date))
  
  pos_test_ratio_as_pct <- paste0(
    as.character(round(df$pos_tests_ratio, 2) * 100) , "%")
  
  p <- plot_ly(x = ~df$region_name, 
                 y = ~df$pos_tests_ratio, 
                 type = 'bar', text = pos_test_ratio_as_pct, 
                 marker = list(color = 'rgb(158,202,225)', 
                               line = list(color = 'rgb(8,48,107)', width = 1.5)))
  p <- p %>% layout(title = list(text = paste0('Percentage of positives over tests by region',
                                               '<br>',
                                               '<sup>',
                                               as.character(max(df$date)),'</sup>')),
                        xaxis = list(title = ""),
                        yaxis = list(title = ""))
  
  p
}
