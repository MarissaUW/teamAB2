library(shiny)
library(ggplot2)
library(plotly)

crime_set <- as.data.frame(read.csv("5yr_Crime_Data.csv"))
crime_categories <- unique(crime_set$Crime.Subcategory)
neighborhoods <- unique(crime_set$Neighborhood)

my_ui <- fluidPage(
  titlePanel("Crime in Seattle Neighborhoods"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("crime", "Crime",
                  choices = crime_categories),
      dateRangeInput("date", "Date Range",
                     format = "mm/dd/yyyy",
                     start = "01/01/2014",
                     end = "12/31/2017",
                     min = "01/01/2014",
                     max = "01/01/2018",
                     startview = "year",
                     weekstart = 0),
      sliderInput("time_range", "Time Range",
                  range(crime_set$Occurred.Time),
                  min = 0,
                  max = 24)
    ),
    mainPanel(
      plotOutput("neighborhoodCrimePlot")
    )
  )
)

shinyUI(my_ui)

