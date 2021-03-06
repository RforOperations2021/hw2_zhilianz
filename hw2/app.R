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

# Read data and convert character value to numeric value in order to calculate min and max

data <- read.csv("data.csv",col.names = c("state", "case","death","fatality","weeklynewcase","vaccineadministered"))
data$case <- as.numeric(gsub(",","",data$case))
data$death <- as.numeric(gsub(",","",data$death))
data$fatality <- as.numeric(gsub("%","",data$fatality))
data$weeklynewcase <- as.numeric(gsub(",","",data$weeklynewcase))
data$vaccineadministered <- as.numeric(gsub(",","",data$vaccineadministered))

ui <- dashboardPage(
    dashboardHeader(title = "COVID-19 Statistics"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Gauges", tabName = "gauge", icon = icon("dashboard")),
            menuItem("Charts", tabName = "chart", icon = icon("chart-bar")),
            menuItem("Analysis",tabName = "analyze", icon=icon("diagnoses")),
            menuItem("Data Table", tabName = "datatable", icon = icon("table"))
            
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content, display basic statistics from five angles in gauge
            tabItem(tabName = "gauge",
                    selectInput("select_state","Please select the state you want to view", 
                                choices = data$state,
                                selected = "Pennsylvania"),
                    fluidRow(
                        
                        column(6,
                               h4("Total case within the state"),
                               flexdashboard::gaugeOutput("gauge1")),
                        column(6,
                               h4("Total death"),
                               flexdashboard::gaugeOutput("gauge2")),                        
                        
                        fluidRow(
                            column(4,
                                   h4("Fatality Rate"),
                                   flexdashboard::gaugeOutput("gauge3")),                        
                            
                            column(4,
                                   h4("Weekly new case"),
                                   flexdashboard::gaugeOutput("gauge4")),                        
                            
                            column(4,
                                   h4("Vaccine administered"),
                                   flexdashboard::gaugeOutput("gauge5")),                        
                        )
                    )
                    
            ),
            
            
            # Second tab content
            tabItem(tabName = "chart",
                    fluidRow(
                        column(4,
                               sliderInput("size",
                                           "How many states you want to select",
                                           min=1, 
                                           max = 40,
                                           value = 10),
                        ),
                        column(4,
                               plotOutput("plot1")
                               
                        ),
                        column(4,
                               plotOutput("plot2")
                               
                        ),
                        column(4,offset = 4,
                               plotOutput("plot3")
                               
                        ),
                        column(4,
                               plotOutput("plot4")
                               
                        )
                    )
                    
                    
                    
            ),
            # third tab content
            tabItem(tabName = "analyze",
                    fluidRow(
                        column(4,
                               h4("You can select two statistics and plot them to see the relationship between two stats"),
                               selectInput("select_stat","Select a statistics you want",choices = colnames(data)[2:6],selected = 'case'),
                               selectInput("select_stat2","Select 2nd statistics you need to analyze",choices = colnames(data)[2:6],selected= 'death'),
                               
                        ),
                        
                        column(4,
                               plotOutput("plot5")
                               
                        )
                    )            
            ),
            # fourth tab content
            tabItem(tabName = "datatable",
                    DT::dataTableOutput("table")
            )
        )
    )
)

server <- function(input, output) {
    
    observeEvent(input$select_state,{
        
        state_selected = input$select_state
        
        output$gauge1 <- flexdashboard::renderGauge({
            gauge(data$case[which(data$state == state_selected)], min = min(data$case), max = max(data$case)) 
        })  
        output$gauge2 <- flexdashboard::renderGauge({
            gauge(data$death [which(data$state == state_selected)],min = min(data$death ), max = max(data$death ),
                  sectors = gaugeSectors(
                      success = c(0, 50),
                      warning = c(50, 80),
                      danger = c(80, 100)
                  ))         
        })  
        output$gauge3 <- flexdashboard::renderGauge({
            gauge(data$fatality[which(data$state == state_selected)],symbol = '%',min = min(data$fatality), max = max(data$fatality),
                  sectors = gaugeSectors(
                      success = c(0, 50),
                      warning = c(50, 80),
                      danger = c(80, 100)
                  ))         
        })  
        output$gauge4 <- flexdashboard::renderGauge({
            gauge(data$weeklynewcase[which(data$state == state_selected)],min = min(data$weeklynewcase), max = max(data$weeklynewcase),
                  sectors = gaugeSectors(
                      success = c(0, 50),
                      warning = c(50, 80),
                      danger = c(80, 100)
                  ))         
        })  
        output$gauge5 <- flexdashboard::renderGauge({
            gauge(data$vaccineadministered[which(data$state == state_selected)],min = min(data$vaccineadministered), max = max(data$vaccineadministered))         
        })  
        
    })
    
    
    observeEvent(input$size,
                 selected_data <<- data %>% sample_n(input$size, replace = FALSE),
                 
    )
    
    observeEvent(c(input$size,input$select_stat,input$select_stat2),{
        
        output$plot1 <- renderPlot({
            slices <- selected_data$case
            pie(slices,label = selected_data$state,main = "Total cases proportion in selected states")
            
        })
        output$plot2 <- renderPlot({
            ggplot(selected_data, aes(x =  state, y =death)) + geom_col(fill ="lightblue") +ggtitle("Death cases in selected states")
            
            
        })
        output$plot3<- renderPlot({
            ggplot(selected_data, aes(x =  fatality, y = state)) + geom_point(size = 5, aes(color = 'darkblue'))+ggtitle("Fatality rate in selected states")
            
            
        })
        output$plot4 <- renderPlot({
            ggplot(selected_data, aes(x = weeklynewcase, y = state)) + geom_col(size = 3)+ggtitle("Weekly new cases in selected states")
            
            
        })
        output$plot5 <- renderPlot({
            ggplot(selected_data, aes_string(x = input$select_stat, y = input$select_stat2 , fill = "state")) +
                geom_point(stat = "identity", size=5,aes(color=state)) +
                xlab(input$select_stat) +
                ylab(input$select_stat2) +
                guides() +
                theme_bw()
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
