library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(" Broad Crime Count Type by Precinct and Year"),
  
  # Sidebar with a select input for year and select input for select neighborhood
  sidebarLayout(
    sidebarPanel(
      selectInput("precinct", "Neighborhood:", choices = unique(crime_data$Precinct)),
      selectInput("year", "Year:", choices = c(2014,2015,2016,2017,2018))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("Plot"), 
       plotOutput("Plot2")
    )
  )
))
