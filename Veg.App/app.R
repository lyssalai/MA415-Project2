library(tidyverse)
library(zoo)
library(shiny)
library(shinydashboard)
library(rsconnect)

# load veg_data3.csv
GenChem <- read.csv("General Chem Data.csv")
GenChem <- as.tibble(GenChem)
GenChem <- GenChem %>% filter(kind == "RESTRICTED USE CHEMICAL", Unit == " MEASURED IN LB")


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Shiny app"),
  dashboardSidebar(    
    sidebarMenu(
      menuItem("Vegetable", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "widgets",
              fluidRow(
                box(
                  selectInput("a", "Choose commodity:",choices = c(2006,2010,2014,2016)),
                  selectInput("b", "Choose commodity:",choices = c("BROCCOLI","CAULIFLOWER"))
                ),
                box(
                  title = "Chemcials and Toxicity",
                  status= "success",
                  solidHeader = TRUE,
                  plotOutput("graph4"),              
                  hr(),
                  helpText("Data from EPA"),
                  verbatimTextOutput("dateText")
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  a <- reactive({
    filter(data_veg, Year==input$a) %>% filter(Commodity == input$b)
  })
  
  output$graph4 <- renderPlot({
    ggplot(d(), mapping=aes(x= Name, y=Value )) + 
      geom_bar(stat="identity", position="dodge") + 
      coord_flip()+
      labs(y = "Value(lb) ",x = "Chemical")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)