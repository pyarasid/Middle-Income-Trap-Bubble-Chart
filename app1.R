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
                  selected=levels(wdiApp$WB.Region),
                  multiple = TRUE),
      
      selectizeInput(inputId = "nation",
                     label="Choose one or more countries",
                     choices=wdiApp$country,
                     options = list(placeholder='select a country name'),
                     multiple=TRUE),
      br(), 
      helpText("The size of each point represents the total population during that year."), 
      br(),
      p("The World Bank determines the threshold for classification of countries by Gross National Income per capita. New thresholds are determined at the start of the Bank’s fiscal year in July and remain fixed for twelve months regardless of subsequent revisions to estimates. "),
      br(),
      p("Low Income: below $995"),
      br(),
      p("Lower Middle Income: $996 to $3,895"),
      br(),
      p("Upper Middle Income: $3,896 to $12,055"),
      br(),
      p("High Income: above $12,055")
    ),
    mainPanel(
      plotlyOutput(outputId = "scatterplot", width = "100%", height = "550px")
    )
  )
)

server <- function(input, output){
  plotdata <- reactive({
    cc <-  wdiApp[complete.cases(wdiApp),] 
    z <- subset(cc, WB.Region %in% input$WB)
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
    reactivestatement2 <- reactive({
      cc <- wdiApp[complete.cases(wdiApp),]
      z <- subset(cc, cc$country %in% input$nation)
      return(z)
    })
    
   output$scatterplot <- renderPlotly({
        ggplotly(ggplot(data = plotdata(),mapping = aes(x=GNI, y=GDP/1000000000, color=WB.Region))+
                   geom_text(data = reactivestatement2(), aes(label=country,frame=Years, ids=country), nudge_x = 0, nudge_y = 0.3)+
                   geom_point(data = reactivestatement2(),aes(size=POP,frame=Years, ids=country, color=WB.Region))+
                   
                   geom_point(aes(size=POP,frame=Years, ids=country, color=WB.Region), alpha=0.2) +
                   geom_vline(xintercept = c(995, 3895, 12055), size=0.3, color="yellow")+
                   
                   scale_x_log10(breaks=c(995,3895, 12055), labels=comma)+scale_y_log10(labels=comma)+
                   labs(x="GNI per capita, (current US$)", y="GDP (constant 2010 US$), in billions", color="World Bank Region")+
                   theme_bw())
              
        })
 
      })
    }


#Create the shiny app object
shinyApp(ui=ui, server=server)
