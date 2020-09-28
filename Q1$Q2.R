library(shiny)
library(datasets)
library(ggplot2) 
library(leaflet)
library(dplyr)

#reading the file 
mydata <- read.csv("assignment-02-data-formated.csv",header=T)

#changing the value column as numeric 
mydata[,6] <-  as.numeric(sub("%", "",mydata$value,fixed=TRUE))

#plot of different types of corals bleaching over period of time
#arranged according to latitude
ggplot(mydata,aes(year,value)) +
  geom_point() + ggtitle("Different types of corals bleaching at different locations over a period of time")+
  facet_grid(coralType~reorder(location,latitude))+
  geom_smooth(
  method = "lm",
  color = "blue",se=FALSE)














