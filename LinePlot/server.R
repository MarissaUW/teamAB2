#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)

shinyServer(function(input, output) {
  
  # Read in data csv file 
  crime_data <- read.csv("../data/5yr_Crime_Data.csv", stringsAsFactors = FALSE)
  
  # Create widget that allows user to select a neighborhood in Seattle
  output$neighborhood <- renderUI({
    selectInput("neighborhood_choice", "Select a Neighborhood", c(unique(crime_data$Neighborhood)))
  })
  
  # Create widget that allows user to select a month (this month's data is displayed)
  output$month <- renderUI({
    selectInput("month_choice", "Select a Month", c("January" = "01", "February" = "02", "March" = "03", "April" = "04",
                                                    "May" = "05", "June" = "06", "July" = "07", "August" = "08", 
                                                    "September" = "09", "October" = "10", "Novemeber" = "11", "December" = "12"))
  })
  
  # Creates line ggplot with information user selected (neighborhood & year) and displays the count of crimes
  # that occured per day (if any) 
  output$linePlot <- renderPlot({
    neighborhood <- input$neighborhood_choice
    year <- input$year_choice
    month <- input$month_choice
    plot_data <- crime_data %>% group_by(Occurred.Date) %>% filter(Neighborhood == neighborhood, endsWith(Occurred.Date, year),
                 startsWith(Occurred.Date, month)) %>% count()
    ggplot(plot_data, aes(x=Occurred.Date, y=n, group=1)) +
      geom_line()+
      geom_point() + 
      labs(title = "Crime Counts per Day", x = "Day", y = "Crime Count") + 
      theme(axis.text.x = element_text(angle = 30))
  })
  
  
})
