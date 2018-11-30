#load relevant libraries
library(tidyverse)
library(ggplot2)
library(plotly)
library(shiny)
library(dplyr)
library(scales)
library(readxl)


wdiApp <- read_excel("wdiApp.xlsx")
#======================================================
#to get the distict values in a column
#(wdiApp %>% distinct(WB.Region))$WB.Region
#or
#wdiApp %>% distinct(WB.Region)
#=========================================================

#create the app =========================================

#ui===================================================

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "WB",
                  label = "Select one of more regions:",
                  choices=wdiApp$WB.Region,
                  select = c("Sub-Saharan Africa", "South Asia","Europe and central asia","Middle east and north africa",
                               "East asia and pacific","Latin america and caribbean","North America"),
                  multiple = TRUE),
      
      selectizeInput(inputId = "nation",
                     label="Choose one or more countries",
                     choices=levels(wdiApp$country),
                     options = list(placeholder='select a country name'),
                     multiple=TRUE)
    ),
    mainPanel(
      plotlyOutput(outputId = "scatterplot")
    )
  )
)

server <- function(input, output) {
  #create a reactive function
  plotdata <- reactive({
    wdiApp %>% filter(WB.Region %in% input$WB) %>% 
      filter(country %in% input$nation)
  })
  #create the bubble chart
  output$scatterplot <- renderPlotly({
  ggplotly(ggplot(data = plotdata(),mapping = aes(x=GNI, y=GDP, colour=WB.Region))+
      geom_point(aes(size=POP,frame=Years, ids=country), alpha=0.7)
  )
  })
}

#Create the shiny app object
shinyApp(ui=ui, server=server)
