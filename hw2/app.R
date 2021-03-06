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
data$population <- as.numeric(gsub(",","",data$population))
data$crime <- as.numeric(gsub(",","",data$crime))
data$murder <- as.numeric(gsub(",","",data$murder))
data$crimerate <- as.numeric(gsub(",","",data$crimerate))
data$murderrate <- as.numeric(gsub(",","",data$murderrate))




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
                        h4("Total population of each city"),
                        flexdashboard::gaugeOutput("gauge1"),
                        
                        h4("Crime rate calculated per 100,000 people"),
                        flexdashboard::gaugeOutput("gauge2"),
                        
                        h4("Murder crime rate calculated per 100,000 people"),
                        flexdashboard::gaugeOutput("gauge3")
                    )
                    
            ),
            
            
            # Second tab content
            tabItem(tabName = "chart",
                    sliderInput("size",
                                "How many cities you want to select",
                                min=1, 
                                max = 26,
                                value = 10),
                    fluidRow(
                        box(
                            plotOutput("plot1")
                            
                        ),
                        box(
                            plotOutput("plot2")
                            
                        ),
                        box(
                            plotOutput("plot3")
                            
                        )
                        
                    )
            ),
            
            
            # third tab content
            tabItem(tabName = "datatable",
                    DT::dataTableOutput("table")
            )
        )
    )
)

server <- function(input, output) {

    observeEvent(input$select_city,{
        city_selected = input$select_city
        output$gauge1 <- flexdashboard::renderGauge({
            gauge(data$population[which(data$city == city_selected)], min = min(data$population), max = max(data$population)
            ) 
        })  
        output$gauge2 <- flexdashboard::renderGauge({
            gauge(data$crimerate[which(data$city == city_selected)],min = min(data$crimerate), max = max(data$crimerate),
                  sectors = gaugeSectors(
                      success = c(0, 50),
                      warning = c(50, 80),
                      danger = c(80, 100)
                  ))         })  
        output$gauge3 <- flexdashboard::renderGauge({
            gauge(data$murderrate[which(data$city == city_selected)], symbol = '%',min = min(data$murderrate), max = max(data$murderrate),
                  sectors = gaugeSectors(
                      success = c(0, 50),
                      warning = c(50, 80),
                      danger = c(80, 100)
                  ))         })  
        
    })
    
    observeEvent(input$size,{
        selected_data <<- data %>% sample_n(input$size, replace = FALSE)

        output$plot1 <- renderPlot({
            ggplot(selected_data, aes(x = population , y = city)) + geom_col(fill ="pink") +ggtitle("Total Population")
            
        }) 
        output$plot2 <- renderPlot({
            ggplot(selected_data, aes(x = population , y = city)) + geom_col(fill ="green") +ggtitle("Crime")
            
        }) 
        output$plot3 <- renderPlot({
            ggplot(selected_data, aes(x = population , y = city)) + geom_col(fill ="yellow") +ggtitle("Crime")
            
        }) 
        output$table = DT::renderDataTable(
            DT::datatable(data = selected_data,
                          options = list(pageLength = 10),
                          rownames = FALSE
            )
            
        )
        
    })
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)
