  #
  # This is a Shiny web application. You can run the application by clicking
  # the 'Run App' button above.
  #
  # Find out more about building applications with Shiny here:
  #
  #    http://shiny.rstudio.com/
  #
  library(ggplot2)
  library(shiny)
  #install.packages("shinydashboard")
  library(shinydashboard)
  library(reshape2)
  library(leaflet)
  
  
  avg_AT <- read.csv("avg_AT.csv")
  avg_temps <- read.csv("avg_temps.csv")
  avg_ST <- read.csv("avg_ST.csv")
  
  ui <- dashboardPage(
    dashboardHeader(title="Average Temperatures"),
    dashboardSidebar(
      sidebarMenu(
      menuItem("Buoy 46035", tabName = "temp", icon = icon("th"))
      )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "temp",
                  fluidRow(
                    box(
                      title = "Air Temp Bar Plot for Buoy 46035 in the Bering Sea",
                      plotOutput("graph1")),
                  box(
                    title = "Water Temp Bar Plot for Buoy 46035 in the Bering Sea", 
                    plotOutput("graph2")
                  )),
                  fluidRow(
                    box(
                      title="Average Air and Sea Temperatures From 1988 to 2017",
                      plotOutput("graph4"),
                      hr(),
                      helpText("Data from NOAA")
                    ),
                      box(
                          title = "Overview of Mean Temperatures",
                          verbatimTextOutput("summarybox"),
                          helpText("The above data summarizes mean temperatures from buoy 46035 from
                                   the years 1988 to 2017.")),
                      infoBoxOutput("sourcebox"),
                    
                  box(title = "Map of Buoy 46035",
                      leafletOutput("buoy_map")
                    
                  )
                  )
                 
                  
                  
          )
        )
    )
        )
          
  
  
  # Define server logic required to draw a histogram
   server <- function(input, output) {
     
     output$summarybox <- renderPrint({
       summary(dataset)
     })
  
     output$graph4 <- renderPlot({
       ggplot(avg_temps, aes(x = year)) + 
         geom_line(aes(y = avg_AT, color ="Air Temperature")) + 
         geom_line(aes(y = avg_ST, color = "Water Temperature")) +
         xlab("Year") +
         ylab("Temperature (Degree Celsius)")
       })
     
     output$graph1 <- renderPlot({
       dataset <- as.data.frame(avg_tempr)
       avgtemp <- dataset[, 1:2]
       ggplot(avg_AT,aes(year,avg_AT,fill=year))+geom_col()+labs(y="Average Air Temperature (deg C)", title="Average Air Temperature by Year")+ theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(1988, 2017, 2))
       
     })
     output$graph2 <- renderPlot({
       ggplot(avg_ST,aes(year,avg_ST,fill=year))+geom_col()+labs(y="Average Sea Temperature (deg C)",title="Average Sea Temperature by Year")+ theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(1988, 2017, 2))
     })
     
     output$buoy_map <- renderLeaflet({
       m <- leaflet() %>% addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png", attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>% setView(177.738, 57.026, zoom = 4) %>% addMarkers(lng=177.738, lat=57.026, popup="The Buoy (ID:46035)") 
       m
     })
   }
   
  
  # 
  # Run the application
  shinyApp(ui = ui, server = server)
  
