library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(" Specific Crime Count Type by Neighborhood and Year"),
  
  # Sidebar with a select input for year and select input for select neighborhood
  sidebarLayout(
    sidebarPanel(
      selectInput("neighborhood", "Neighborhood:", choices = unique(crime_data$Neighborhood)),
      selectInput("year", "Year:", choices = c(2014,2015,2016,2017,2018))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("Plot")
    )
  )
))
