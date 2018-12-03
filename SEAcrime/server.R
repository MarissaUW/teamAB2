library(shiny)
library(leaflet)
library(dplyr)

beat.sf2 <- readRDS("data/beat.sf2.Rds")
df_map <- readRDS("data/clean_calldata.Rds")

server <- function(input, output, session) {
  data <- reactive({
    x <- beat.sf2
  })
  ## Interactive Map ###########################################

  #Crate map
  output$map <- renderLeaflet({
    beat.sf2 <- data()

    m2 <- leaflet() %>% 
      addProviderTiles(providers$Stamen) %>% 
      addPolygons(data = beat.sf2, weight = 1, smoothFactor = 0.75, opacity = 0, fillOpacity = 0.6, fillColor = ~factpal(beat.sf2$beat)) 
    m2 
  })
  points <- eventReactive(input$Start, {
    if(input$Month<=9){M=paste0("0",input$Month)}else{M=input$Month}
       }
    )
}
