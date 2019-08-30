library(shiny)
library(leaflet)
library(ggplot2)
library(maptools)
library(rgeos)

ui <- bootstrapPage(
  br(),
  column(8,leafletOutput("map",  height="800px")),
  
  column(4,plotOutput("plot", click = "clickMap", height="350px"),
         h4(textOutput("Country")),
         h4(textOutput("Region")),
         h4(textOutput("GDP")),
         h4(textOutput("Fertility")),
         h5(textOutput("Noteline")),
         h6(textOutput("Note1")),
         h6(textOutput("Note2")),
         h6(textOutput("Note3"))),
  br()
)

