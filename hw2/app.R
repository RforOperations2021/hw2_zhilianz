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

data <- read.csv("data.csv",col.names = c("city", "population","crime","murder","crimerate","murderrate"))


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
                    selectInput("select_city")
                    
                    
            ),
            
            
            # Second tab content
            tabItem(tabName = "chart",
                    sliderInput("size",
                                "How many states you want to select",
                                min=1, 
                                max = 40,
                                value = 10),
                    h2("Widgets tab content")
            ),
            
            
            # third tab content
            tabItem(tabName = "datatable",
                    DT::dataTableOutput("table")
            )
        )
    )
)

server <- function(input, output) {
    
    output$table = DT::renderDataTable(
        if (input$table){
            DT::datatable(data = data,
                          options = list(pageLength = 10),
                          rownames = FALSE
            )
        }
    )
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
