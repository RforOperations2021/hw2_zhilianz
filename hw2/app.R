# Shiny R for Operations
# Homework 2
# Andrew ID: zhilianz


library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(shinyBS)
library(shinydashboard)
library(flexdashboard)

data <- read.csv("data.csv",col.names = c("city", "population","crime","murder","crimerate","murderrate"),row.names = NULL,stringsAsFactors=FALSE)


ui <- dashboardPage(
    dashboardHeader(title = "Crime Rate Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Gauges", tabName = "gauge", icon = icon("dashboard")),
            menuItem("Charts", tabName = "chart", icon = icon("th")),
            menuItem("Data Table", tabName = "datatable", icon = icon("table"))
            
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "gauge",
                    selectInput("select_city","Please select the city you want to view", 
                                choices = data$city),
            fluidRow(
                flexdashboard::gaugeOutput("gauge1"),
                flexdashboard::gaugeOutput("gauge2"),
                flexdashboard::gaugeOutput("gauge3")
            )
                    
            ),
            
            
            # Second tab content
            tabItem(tabName = "chart",
                    sliderInput("size",
                                "How many cities you want to select",
                                min=1, 
                                max = 40,
                                value = 10),
                    h2("Widgets tab content")
            ),
            
            
            # third tab content
            tabItem(tabName = "datatable",
                    checkboxInput("show","Show Table",value = FALSE),
                    DT::dataTableOutput("table")
            )
        )
    )
)

server <- function(input, output) {
    observeEvent(input$select_city,{
        city_selected = input$select_city
        output$gauge1 <- flexdashboard::renderGauge({
            gauge(data$population[which(data$city == city_selected)], min = min(data$population), max = max(data$population),       sectors = gaugeSectors(
                success = c(20, 80),
                warning = c(10, 90),
                danger = c(0, 100)
            )) 
        })  
        output$gauge2 <- flexdashboard::renderGauge({
            gauge(median(diamonds$price), min = 0, max = max(diamonds$price), symbol = " Dollar") 
        })  
        output$gauge3 <- flexdashboard::renderGauge({
            gauge(median(diamonds$price), min = 0, max = max(diamonds$price), symbol = " Dollar") 
        })  
        
        
    })

    
    output$table = DT::renderDataTable(
        if (input$show){
            DT::datatable(data = data,
                          options = list(pageLength = 10),
                          rownames = FALSE
            )
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
