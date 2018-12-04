library(shiny)
library(ggplot2)
library(plotly)

##crime_set <- as.data.frame(read.csv("~/Desktop/Info-201/Final/teamAB2/data/5yr_Crime_Data.csv"))
crime_set <- as.data.frame(read.csv("5yr_Crime_Data.csv"))
crime_categories <- unique(crime_set$Crime.Subcategory)
neighborhoods <- unique(crime_set$Neighborhood)

my_ui <- fluidPage(
  titlePanel("Crime in Seattle Neighborhoods"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("data_choice", "Data to View:",
                   c("View All" = "all",
                     "View Single Neighborhood" = "single_neighborhood")),
      useShinyjs(),
      disabled(
        selectInput("neighborhood", "Neighborhood",
                    choices = neighborhoods)
      ),
      dateRangeInput("date", "Date Range",
                     format = "mm/dd/yyyy",
                     start = "01/01/2014",
                     end = "12/31/2017",
                     min = "01/01/2014",
                     max = "01/01/2018",
                     startview = "year",
                     weekstart = 0),
      sliderInput("time_range", "Time Range (Hours)",
                 range(crime_set$Occurred.Time),
                 min = 0,
                 max = 24)
    ),
    mainPanel(
      plotOutput("crimePlot")
    )
  )
)

shinyUI(my_ui)

