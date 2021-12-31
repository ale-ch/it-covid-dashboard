library(shiny)
library(lubridate)
library(tidyverse)

source("https://raw.githubusercontent.com/ale-ch/it_covid_dashboard/main/dataset_create.R")
source("https://raw.githubusercontent.com/ale-ch/it_covid_dashboard/main/covid_plot.R")

regions <- c(NULL, levels(as.factor(covid_italy$denominazione_regione)))
max_days <- length(seq(min(covid_italy$data), max(covid_italy$data), by = "days"))

selected_names <- names(covid_italy) %in% c("data", "stato", "denominazione_regione", 
                                           "codice_regione", "lat","long")
selected_names <- names(covid_italy)[selected_names == FALSE]

ui <- fluidPage(

        titlePanel("Italy Covid Dashboard"),
        
        fluidRow(
          column(6,
            sliderInput("last_n_days", "Select number of days from current date:", value = 14, 
                        min = 1, max = max_days)
                 ),
          column(6,
            selectInput("variable", "Select variable to visualize:", 
                        choices = selected_names)
          )
        ),

        checkboxGroupInput("region", "Select region(s):", regions,
                           inline = TRUE,
                           selected = "Nazionale"),
        
        fluidRow(
          column(4, 
            radioButtons("variation", "Visualize variation:", c(FALSE, TRUE))
          ),
          column(4, 
            radioButtons("percent", "Visualize in percentages:", c(FALSE, TRUE))
          )
        ),

        plotOutput("plot", width = "100%"),
        dataTableOutput("static")
 
    )


server <- function(input, output, session) {

    output$plot <- renderPlot({
        covid_plot(df = covid_italy,
                   var = input$variable, 
                   last_n_days = input$last_n_days,
                   variazione = input$variation,
                   percentuale = input$percent, 
                   regione = input$region)
    })
    
    output$static <- renderDataTable({
      
      covid_italy %>% 
        group_by(denominazione_regione) %>% 
        filter(
          data == max(data),
          denominazione_regione %in% input$region) %>% 
        summarise(
          date = data,
          Rt = round(Rt, 2),
          new_infections = nuovi_positivi,
          new_tests = nuovi_tamponi,
          infections_tests_ratio = paste0(as.character(round(
            ratio_positivi_tamponi * 100), 2),"%"),
          ratio_confint_95 = ratio_confint_95)
    })
}

shinyApp(ui = ui, server = server)
