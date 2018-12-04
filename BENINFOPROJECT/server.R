

library(shiny)
library(dplyr)
library(ggplot2)

# Define server logic required to draw a plot
shinyServer(function(input, output) {
  #read CSV file into data frame
  readData <- crime_data <- read.csv("../data/5yr_Crime_Data.csv")
  #define plot
  output$Plot <- renderPlot({
    
      # filter data frame into correct frame for plotting
      data1 <- readData %>% 
      select(Occurred.Date, Primary.Offense.Description, Neighborhood)%>%
      mutate(Year = substr(Occurred.Date, 7, 10))%>%
      select(Primary.Offense.Description, Neighborhood, Year)%>%
      filter(Neighborhood == input$neighborhood & Year == input$year)%>%
      group_by(Primary.Offense.Description)%>%
      count()%>%
      data.frame()
      
      #plot
      ggplot(data1, aes(x=Primary.Offense.Description, y=n) ) +
        geom_bar(stat="identity", fill="#49c9d4") +
        coord_flip()+
        xlab("Type of Crime") +
        ylab("Number of Incidents")
  })
  
})

