library(shiny)

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
            selectInput("variable", "Select variable to visualize:", selected_names,
                        selected = "Nazionale")
          )
        ),

        checkboxGroupInput("region", "Select region(s):", regions,
                           inline = TRUE),
        
        fluidRow(
          column(4, 
            radioButtons("variation", "Visualize variation:", c(FALSE, TRUE))
          ),
          column(4, 
            radioButtons("percent", "Visualize in percentages:", c(FALSE, TRUE))
          )
        ),
        
        fluidRow(
          column(8, 
            plotOutput("plot", width = "100%")
          ),
          column(4,
            verbatimTextOutput("text1")
          )
        )
    )


server <- function(input, output, session) {

    output$plot <- renderPlot({
        covid_plot(df = covid_italy,
                   var = input$variable, 
                   last_n_days = input$last_n_days,
                   variazione = input$variation,
                   percentuale = input$percent, 
                   regione = input$region
                   )
    })
    
    output$text1 <- renderPrint({
      writeLines(
        paste0("date: ", as.character(max(covid_italy$data)), "\n",
               
               "Rt: ", as.character(round(covid_italy$Rt[nrow(covid_italy)], 2)), "\n",
               "new infections:" , as.character(covid_italy$nuovi_positivi[nrow(covid_italy)]), "\n",
               "number of tests: ", as.character(covid_italy$variazione_tamponi[nrow(covid_italy)]), "\n",
               "infections-tests ratio (%): ", 
               as.character(round(covid_italy$ratio_positivi_tamponi[nrow(covid_italy)] * 100), 2), "%"
               ))
        
    })
}

shinyApp(ui = ui, server = server)
