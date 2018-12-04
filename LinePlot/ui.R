#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Seattle Crime"),
  
  sidebarLayout(
    sidebarPanel(
      # Creates Radio Buttons that allow the user to select a year (between 2014 and 2018)
      radioButtons("year_choice", "Choose a Year:", c(2014, 2015, 2016, 2017, 2018),
                   selected = 2014),
      # Displays the "month" widget 
      uiOutput("month"),
      # Displays the "neighborhood" widget 
      uiOutput("neighborhood")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("linePlot")
    )
  )
))
