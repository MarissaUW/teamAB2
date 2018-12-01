library(shiny)
library(leaflet)


ui <- navbarPage("Police Activity in Seattle",
           
  tabPanel("Data explorer"),
    fluidRow(
      column(3
        selectInput("neighborhood", "Neighborhood", c("All neighborhoods"="", structure()))
        )
    )
           
           
           
           tabPanel("Interactive map"),
           tabPanel("Ben"),
           tabPanel("Claire"),
           tabPanel("Paola")
)

