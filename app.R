library(shiny)
library(lubridate)
library(tidyverse)

source("https://raw.githubusercontent.com/ale-ch/it_covid_dashboard/main/dataset_create.R")
source("https://raw.githubusercontent.com/ale-ch/it_covid_dashboard/main/covid_plot.R")

regions <- c(NULL, levels(as.factor(covid_italy$region_name)))
max_days <- length(seq(min(covid_italy$date), max(covid_italy$date), by = "days"))

selected_names <- names(covid_italy) %in% c("date", "region_name", "ratio_confint_95")
selected_names <- names(covid_italy)[selected_names == FALSE]

ui <- fluidPage(
  
        theme = bslib::bs_theme(bootswatch = "journal"),

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

        
        sidebarLayout(
          
          sidebarPanel = sidebarPanel(
            checkboxGroupInput("region", "Select region(s):", regions,
                               inline = FALSE,
                               selected = "National")
            
          ),
          
          mainPanel = mainPanel(
            
            fluidRow(
              column(4, 
                     checkboxInput("variation", "Visualize intraday variation:")
              ),
              column(4, 
                     checkboxInput("percent", "Visualize in percentages:")
              )
            ),
            
            plotOutput("plot", width = "100%"),
            dataTableOutput("static")
          )
        )

        
 
    )


server <- function(input, output, session) {
  
    thematic::thematic_shiny()

    output$plot <- renderPlot({
        covid_plot(df = covid_italy,
                   var = input$variable, 
                   last_n_days = input$last_n_days,
                   variation = input$variation,
                   percentage = input$percent, 
                   region = input$region)
    })
    
    output$static <- renderDataTable({
      
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
            pos_tests_ratio * 100), 2),"%"),
          ratio_confint_95 = ratio_confint_95)
    })
}

shinyApp(ui = ui, server = server)
