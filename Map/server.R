#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Determines the color of the marker based on the number of crimes committed in given neighborhood
  getColor <- function(set){
    sapply(set$n, function(n){
      if (n <= 1000){
        "green"
      } else if (n <= 5000){
        "gold"
      } else if (n <= 10000){
        "orange"
      } else {
        "red"
      }
    })
  }
  
  group_name <- function(set){
    sapply(set$n, function(n){
      if (n <= 1000){
        "<= 1000"
      } else if (n <= 5000){
        "1001 - 5000"
      } else if (n <= 10000){
        "5001 - 10000"
      } else{
        "10001 - 23024"
      }
    })
  }
   
  output$mymap <- renderLeaflet({
    crime_data <- read.csv("crime_data.csv", stringsAsFactors = FALSE)
    neighborhoods <- crime_data %>% select(Neighborhood, long, lat, n) %>% distinct()
    
    # Filter out values based on chosen values 
    #neighborhoods <- neighborhoods %>% filter(n <= as.numeric(input$counts))
    leaf_map <- leaflet(neighborhoods) %>% 
        addTiles() %>% 
        addCircleMarkers(~long, ~lat, label = ~paste(Neighborhood, "(Num of Crimes:",n,")"), radius = 5,
        color = getColor(neighborhoods), group = group_name(neighborhoods)) %>% 
        addLayersControl(overlayGroups = c("<= 1000", "1001 - 5000", "5001 - 10000", "10001 - 23014"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        setView(lng=-122.3321, lat=47.6062, zoom = 10)
  })
})
