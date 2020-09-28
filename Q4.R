library(shiny)
library(datasets)
library(ggplot2) 
library(leaflet)
library(dplyr)

#reading the file 
mydata <- read.csv("assignment-02-data-formated.csv",header=T)

#changing the value column as numeric 
mydata[,6] <-  as.numeric(sub("%", "",mydata$value,fixed=TRUE))

#using leaflet function to show location of site
leaflet(mydata) %>% addTiles() %>% 
  addMarkers(lng = ~longitude,
                    lat = ~latitude,
                    popup = ~location)
# %>%  is used as a pipe operator
