library(shiny)
library(dplyr)
library(leaflet)


server <- (function(input, output) {
  
  output$map <- renderLeaflet({
    m2 <- leaflet() %>% 
      addProviderTiles(providers$Stamen) %>% 
      addPolygons(data = beat.sf2, weight = 1, smoothFactor = 0.75, opacity = 0, fillOpacity = 0.6, fillColor = ~factpal(beat.sf2$beat))
    m2
  })
})
