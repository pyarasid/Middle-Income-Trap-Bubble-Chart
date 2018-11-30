#load relevant libraries
library(tidyverse)
library(ggplot2)
library(plotly)
library(shiny)
library(dplyr)
library(scales)
library(readxl)

#some manipuation

wdiApp$country <- as.factor(wdiApp$country)
wdiApp$WB.Region <- as.factor(wdiApp$WB.Region)
wdiApp$Years <- as.integer(wdiApp$Years)
wdiApp$GDP <- as.numeric(wdiApp$GDP)
wdiApp$GNI <- as.numeric(wdiApp$GNI)
wdiApp$POP <- as.numeric(wdiApp$POP)

wdiApp <- read_excel("wdiApp.xlsx")
#======================================================
#to get the distict values in a column
#(wdiApp %>% distinct(WB.Region))$WB.Region
#or
#wdiApp %>% distinct(WB.Region)
#cc %>% filter(WB.Region %in% input$WB) %>% filter(country %in% input$nation)
#=========================================================

#create the app =========================================
#select = 
#ui===================================================

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "WB",
                  label = "Select one of more regions:",
                  choices=unique(wdiApp$WB.Region),
                  select=c("Sub-Saharan Africa", "South Asia","Europe and central asia","Middle east and north africa",
                             "East asia and pacific","Latin america and caribbean","North America"),
                  multiple = TRUE),
      
      selectizeInput(inputId = "nation",
                     label="Choose one or more countries",
                     choices=wdiApp$country,
                     options = list(placeholder='select a country name'),
                     multiple=TRUE)
    ),
    mainPanel(
      plotlyOutput(outputId = "scatterplot")
    )
  )
)

server <- function(input, output) {
 observeEvent({
   input$nation
   input$WB
 }, {
   #create reactive functions
  plotdata <- reactive({
    cc <- wdiApp[complete.cases(wdiApp),]
   z <- subset(cc, WB.Region %in% input$WB | cc$country %in% input$nation) 
   return(z)
  })
  plotdata1 <- reactive({
    cc <- wdiApp[complete.cases(wdiApp),]
    z <- subset(cc, cc$country %in% input$nation)
    return(z)
  })
  #create the bubble chart
  if (length(input$nation)>0){
  output$scatterplot <- renderPlotly({
  ggplotly(ggplot(data = plotdata(),mapping = aes_string(x=GNI, y=GDP, color=input$WB))+
      geom_point(aes_string(size=POP,frame=Years, ids=country), alpha=0.2) +
        geom_point(data = plotdata1(),aes_string(size=POP,frame=Years, ids=country, color=input$WB))+
        geom_text(data = plotdata1(), aes_string(label=country,frame=Years, ids=country))+
        scale_x_log10(breaks=c(995,3895, 12055), labels=comma)+scale_y_log10(labels=comma)+
      theme_bw()
     
    )})
  }
  
  else if (length(input$nation==0)) {
    output$scatterplot <- renderPlotly({
      ggplotly(ggplot(data = plotdata(),mapping = aes_string(x=GNI, y=GDP, color=input$WB))+
                 geom_point(aes_string(size=POP,frame=Years, ids=country), alpha=0.7)+
                 scale_x_log10(breaks=c(995,3895, 12055), labels=comma)+scale_y_log10(labels=comma)+
                 theme_bw()
    )
  })
 }
})}

#Create the shiny app object
shinyApp(ui=ui, server=server)
