library(shiny)
library(datasets)
library(ggplot2) 
library(leaflet)
library(dplyr)
#reading the file 
mydata <- read.csv("assignment-02-data-formated.csv",header=T)
#changing the value column as numeric 
mydata[,6] <-  as.numeric(sub("%", "",mydata$value,fixed=TRUE))
#this is the ui.r 

ui <- shinyUI(fluidPage( 
  
  # Application title
  headerPanel("Types of corals bleaching and smoothing at different locations"),
  
  # Sidebar with controls to select the coral type
    sidebarLayout(
    sidebarPanel(
      selectInput("variable1", "Type of Coral:", 
                  c("Blue corals" = "bc", 
                    "Hard corals" = "hc",
                    "Sea fans" = "sf",
                    "Sea pens" = "sp",
                    "Soft corals" = "sc")
      ),
      
      
      # Drop Down with controls to select the smoothing type
      
      selectInput("variable2", "Type of Smoothing:", 
                  c("Linear model" = "lm", 
                    "Generalised linear model" = "glm",
                    "Generalised additive model" = "gam",
                    "Local Regression" = "loess"
                  )
                  
                  
                  
      )
    ),
    mainPanel(
      # Show the caption and plot of the requested coral type and the type of smoothing
      h3(textOutput("caption")),
      plotOutput("plot"),
      #Show the leaflet map
      h3(textOutput("caption2")),
      leafletOutput("map")
          )
  )
))

#this is the server of shiny
server <- shinyServer(function(input, output) {
  
 #caption for the plot of corals 
  output$caption <- reactiveText(function() {
    paste("Coral Bleaching Value")
  })
  
  #saving the plot in this variable plot
  output$plot <- reactivePlot(function() {
   #making a new dataframe with a new column according to coral type
    new <- mydata %>%
      mutate(newcol = case_when(coralType == 'blue corals' ~ 1,
                                coralType == 'hard corals' ~ 2,
                                coralType == 'sea fans' ~ 3,    
                                coralType == 'sea pens' ~ 4,
                                coralType == 'soft corals' ~ 5,
                                TRUE ~ 0))
    #condition check of input variable
    if (input$variable1 == "bc") {
      #creating a new_df (new dataframe) based on input value
      new_df <- new[which(new$newcol == 1),names(new) 
                    %in% c("location",
                           "coralType","longitude"
                           ,"latitude","year","value")]
      
    }
    
    else if (input$variable1 == "hc") {
      new_df <- new[which(new$newcol == 2),names(new) 
                    %in% c("location",
                           "coralType","longitude"
                           ,"latitude","year","value")]
      
    }
    else if (input$variable1 == "sf") {
      new_df <- new[which(new$newcol == 3),names(new) 
                    %in% c("location",
                           "coralType","longitude"
                           ,"latitude","year","value")]
      
    }
    else if (input$variable1 == "sp") {
      new_df <- new[which(new$newcol == 4),names(new) 
                    %in% c("location",
                           "coralType","longitude"
                           ,"latitude","year","value")]
      
    }
    
    else if (input$variable1 == "sc") {
      new_df <- new[which(new$newcol == 5),names(new) 
                    %in% c("location",
                           "coralType","longitude"
                           ,"latitude","year","value")]
    }
    #ploting the graph according to the input variable and giving
    #method of smoothing according to second input
    ggplot(new_df,aes(year,value)) +
      geom_point() + facet_grid(~reorder(location,latitude)) +
      geom_smooth(aes(group = 1),
                  method = input$variable2,
                  color = "red",
                  se = FALSE)
    
  })
  #caption for the plot of location of sites 
  output$caption2 <- reactiveText(function() {
    paste("Location of sites")
  }
  )
  #saving the map into a varaible map
   output$map <- renderLeaflet({
    leaflet(mydata) %>% addTiles() %>%
      addMarkers(lng = ~longitude,
                 lat = ~latitude,
                 popup = ~location,
                 label = ~location,
                 labelOptions = labelOptions(noHide = TRUE, offset=c(0,-10), textOnly = FALSE)) 
 # %>%  is used as a pipe operator
 
  }
  
  )
})

shinyApp(ui, server)




