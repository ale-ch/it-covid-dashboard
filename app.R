library(shiny)
library(plotly)
library(lubridate)
library(tidyverse)
library(DT)

source("generate_dataset.R")
source("line_plot.R")
source("bar_chart1.R")
source("bar_chart2.R")

regions <- c(NULL, levels(as.factor(covid_italy$region_name)))
max_days <- length(seq(min(covid_italy$date), max(covid_italy$date), by = "days"))

selected_names <- names(covid_italy) %in% c("date", "region_name", 
                                            "lat", "long")
selected_names <- names(covid_italy)[selected_names == FALSE]

ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "yeti"),
  
  titlePanel("Italy Covid Dashboard"),
  
  fluidRow(
    column(4,
           sliderInput("last_n_days", 
                       "Select number of days from current date:", value = 14, 
                       min = 1, max = max_days)
    ),
    column(4,
           selectInput("variable", "Select variable to visualize:", 
                       choices = selected_names)
    ),
    
    column(4, 
           selectInput("region", "Select region(s):", regions, multiple = TRUE,
                       selected = "National")
    ),
  ),
  
    
  fluidRow(
    column(4, 
           checkboxInput("variation", "Visualize intraday variation:")
    ),
    column(4, 
           checkboxInput("percent", "Visualize in percentages:")
    )
  ),
  
  plotlyOutput("line_plot"),
  DTOutput("static"),
  br(),
  plotlyOutput("bar_chart1"),
  br(),
  plotlyOutput("bar_chart2"),

)


server <- function(input, output, session) {
  
  thematic::thematic_shiny()
  
  output$line_plot <- renderPlotly({
    line_plot(data = covid_italy,
               var = input$variable, 
               last_n_days = input$last_n_days,
               variation = input$variation,
               percentage = input$percent, 
               region = input$region)
  })
  
  output$bar_chart1 <- renderPlotly({
    bar_chart1(data = covid_italy)
  })
  
  output$bar_chart2 <- renderPlotly({
    bar_chart2(data = covid_italy)
  })
  
  output$static <- renderDT({
    
    covid_italy %>% 
      group_by(region_name) %>% 
      filter(
        date == max(date),
        region_name %in% input$region) %>% 
      summarise(
        date = date,
        Rt = round(Rt, 2),
        new_infections = positives,
        new_tests = tests,
        infections_tests_ratio = paste0(as.character(round(
          pos_tests_ratio * 100), 2),"%"))
  })
}

shinyApp(ui = ui, server = server)
