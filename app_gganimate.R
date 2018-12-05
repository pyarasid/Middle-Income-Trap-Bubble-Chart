#load relevant libraries
library(tidyverse)
library(ggplot2)
library(plotly)
library(shiny)
library(dplyr)
library(scales)
library(readxl)
library(gganimate)

wdiApp <- read_excel("wdiApp.xlsx")

#some manipuation
wdiApp$country <- as.factor(wdiApp$country)
wdiApp$WB.Region <- as.factor(wdiApp$WB.Region)
wdiApp$Years <- as.integer(wdiApp$Years)
wdiApp$GDP <- as.numeric(wdiApp$GDP)
wdiApp$GNI <- as.numeric(wdiApp$GNI)
wdiApp$POP <- as.numeric(wdiApp$POP)


#create the app =========================================

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      
      #select input widgets
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
     
      #slider input to provide "Play" button for Years to users to control the animation
       sliderInput(inputId = "time",
                  label = "Hit play or pause button:",
                  min=1962, max=2017,
                  value=1962, step=1,
                  animationOptions(interval = 1000, loop = TRUE, playButton = "Play", pauseButton = "Pause"),sep = ""),
     
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
      imageOutput(outputId = "scatterplot", width = "100%", height = "550px")
    )
  )
)

server <- function(input, output){
 #Default reactivity when the user doesn't select any country
   plotdata <- reactive({
    cc <-  wdiApp[complete.cases(wdiApp),] 
    z <- subset(cc, WB.Region %in% input$WB)
    return(z)
  })
  #create the bubble chart
  output$scatterplot <- renderImage({
    p <- ggplot(data = plotdata(), mapping = aes(x=GNI, y=GDP/1000000000, color=WB.Region))+
               geom_vline(xintercept = c(995, 3895, 12055), size=0.3, color="yellow")+
               geom_point(aes(size=POP), alpha=0.7) +
               scale_x_log10(breaks=c(995,3895, 12055), labels=comma)+scale_y_log10(labels=comma)+
               guides(size=FALSE)+
               transition_time(Years) +
               labs(title = "Year: {frame_time}", x="GNI per capita, (current US$)", y="GDP (constant 2010 US$), in billions", color="World Bank Region") +
               ease_aes('linear')+ 
               theme_bw()
    
    #saving the output gif file to show in the shiny app
    anim_save("outfile.gif", animate(p))
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif',
         width = 1000,
         height = 700
    )}, deleteFile = TRUE)
    
  
#reactivity when the user selects one or more countries for comparision
  observeEvent(input$nation,{
    reactivestatement2 <- reactive({
      cc <- wdiApp %>% filter(country==input$nation)
      
    })
    
    output$scatterplot <- renderImage({
     p <-  ggplot(data = plotdata(),mapping = aes(x=GNI, y=GDP/1000000000, color=WB.Region))+
                 geom_vline(xintercept = c(995, 3895, 12055), size=0.3, color="yellow")+
                 geom_point(aes(size=POP), alpha=0.2) +
                 geom_point(data = reactivestatement2(),aes(size=POP, color=WB.Region), alpha=1)+
                 geom_text(data = reactivestatement2(), aes(label=country), nudge_x = 0, nudge_y = 0.3)+
                 scale_x_log10(breaks=c(995,3895, 12055), labels=comma)+
                 scale_y_log10(labels=comma)+ guides(size=FALSE)+
                 transition_time(Years) +
                 labs(title = "Year: {frame_time}", x="GNI per capita, (current US$)", y="GDP (constant 2010 US$), in billions", color="World Bank Region") +
                 ease_aes('linear')+
                 theme_bw()
    
      #saving the output gif file to show in the shiny app
     anim_save("outfile.gif", animate(p))
     
     # Return a list containing the filename
     list(src = "outfile.gif",
          contentType = 'image/gif',
          width = 1000,
          height = 700
     )}, deleteFile = TRUE)
    
    
  })
}


#Create the shiny app object
shinyApp(ui=ui, server=server)

    





