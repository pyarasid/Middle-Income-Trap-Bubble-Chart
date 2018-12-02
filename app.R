#load relevant libraries
library(tidyverse)
library(ggplot2)
library(plotly)
library(shiny)
library(dplyr)
library(scales)
library(readxl)
library(ggrepel)

wdiApp <- read_excel("wdiApp.xlsx")

#some manipuation
wdiApp$country <- as.factor(wdiApp$country)
wdiApp$WB.Region <- as.factor(wdiApp$WB.Region)
wdiApp$Years <- as.integer(wdiApp$Years)
wdiApp$GDP <- as.numeric(wdiApp$GDP)
wdiApp$GNI <- as.numeric(wdiApp$GNI)
wdiApp$POP <- as.numeric(wdiApp$POP)

#======================================================
#to get the distict values in a column
#(wdiApp %>% distinct(WB.Region))$WB.Region
#or
#wdiApp %>% distinct(WB.Region)
#cc %>% filter(WB.Region %in% input$WB) %>% filter(country %in% input$nation)
#=========================================================

#create the app =========================================

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "WB",
                  label = "Select one of more regions:",
                  choices=levels(wdiApp$WB.Region),
                  selected=c("Sub-Saharan Africa", "South Asia","Europe and central asia","Middle east and north africa",
                             "East asia and pacific","Latin america and caribbean","North America"),
                  multiple = TRUE),
      
      selectizeInput(inputId = "nation",
                     label="Choose one or more countries",
                     choices=wdiApp$country,
                     options = list(placeholder='select a country name'),
                     multiple=TRUE)
    ),
    mainPanel(
      plotlyOutput(outputId = "scatterplot", width = "100%", height = "550px")
    )
  )
)

server <- function(input, output){
  plotdata <- reactive({
    cc <-  wdiApp[complete.cases(wdiApp),] 
    z <- subset(cc, WB.Region %in% input$WB | cc$country %in% input$nation)
    return(z)
  })
  #create the bubble chart
  output$scatterplot <- renderPlotly({
    ggplotly(ggplot(data = plotdata(), mapping = aes(x=GNI, y=GDP/1000000000, color=WB.Region))+
               geom_vline(xintercept = c(995, 3895, 12055), size=0.3, color="yellow")+
               geom_point(aes(size=POP,frame=Years, ids=country), alpha=0.7) +
               scale_x_log10(breaks=c(995,3895, 12055), labels=comma)+scale_y_log10(labels=comma)+
               labs(x="GNI per capita, (current US$)", y="GDP (constant 2010 US$), in billions", color="World Bank Region")+
               theme_bw())
  })

  observeEvent(input$nation,{
    
    reactivestatement1 <- reactive({
     
      cc <- wdiApp[complete.cases(wdiApp),]
      z <- subset(cc, WB.Region %in% input$WB | cc$country %in% input$nation) 
      return(z)
    })
    reactivestatement2 <- reactive({
      
      cc <- wdiApp[complete.cases(wdiApp),]
      z <- subset(cc, cc$country %in% input$nation)
      return(z)
    })
    
    if (length(input$nation)>0){
      output$scatterplot <- renderPlotly({
        ggplotly(ggplot(data = reactivestatement1(),mapping = aes(x=GNI, y=GDP/1000000000, color=WB.Region))+
                   geom_vline(xintercept = c(995, 3895, 12055), size=0.3, color="yellow")+
                   geom_point(aes(size=POP,frame=Years, ids=country), alpha=0.2) +
                   geom_point(data = reactivestatement2(),aes(size=POP,frame=Years, ids=country, color=WB.Region))+
                   geom_text(data = reactivestatement2(), aes(label=country,frame=Years, ids=country), nudge_x = 0, nudge_y = 0.3)+
                   scale_x_log10(breaks=c(995,3895, 12055), labels=comma)+scale_y_log10(labels=comma)+
                   labs(x="GNI per capita, (current US$)", y="GDP (constant 2010 US$), in billions", color="World Bank Region")+
                   theme_bw())
              
        })
    }
    
    else if (input$nation==FALSE) {
      output$scatterplot <- renderPlotly({
        ggplotly(ggplot(data = reactivestatement1(),mapping = aes(x=GNI, y=GDP/1000000000, color=input$WB))+
                   geom_vline(xintercept = c(995, 3895, 12055), size=0.3, color="yellow")+
                   geom_point(aes(size=POP,frame=Years, ids=country), alpha=0.7)+
                   scale_x_log10(breaks=c(995,3895, 12055), labels=comma)+scale_y_log10(labels=comma)+
                   labs(x="GNI per capita, (current US$)", y="GDP (constant 2010 US$, in billions)", color="World Bank Region")+
                   theme_bw())
      })
    }
  })  
}

#Create the shiny app object
shinyApp(ui=ui, server=server)
