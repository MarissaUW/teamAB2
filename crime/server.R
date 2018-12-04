library(leaflet)
library(leaflet.extras)
library(shiny)


function(input, output, session) {
  output$summary <- renderPrint({
    summary(cars)
  })
  
  output$map <- renderLeaflet({
        leaflet() %>% 
        addProviderTiles(providers$OpenStreetMap.BlackAndWhite, options = providerTileOptions(noWrap = TRUE)) %>%
        setView(lng = -122.335167, lat = 47.608013, zoom = 11)
        addPolygons(data = hood.sf,
                    smoothFactor = 0.75, 
                    opacity = 0, 
                    fillOpacity = 0.6,
                    stroke = FALSE,
                    fillColor = ~factpal3(hood.sf$S_HOOD))
        addCircles(radius = ~2.2*visits, popup = popup, stroke = T,
                fillColor = DarkBlue,
                fillOpacity = 0.75)
  })
  
  output$plot <- renderPlot({
    plot(cars, type=input$plotType)
  })
}
