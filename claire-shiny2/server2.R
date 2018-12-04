library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)

my_server <- function(input, output) {
  
    crime_data <- read.csv("5yr_Crime_Data.csv", stringsAsFactors = FALSE)
  
    output$neighborhoodCrimePlot <- renderPlot({
        crime_choice <- input$crime
        date_choice <- input$date
        dates <- as.Date(crime_data$Occurred.Date, format = "%m/%d/%Y")
        crime_data$Occurred.Date <- dates
        time_choice <- input$time_range
        crimes <- crime_data %>% 
          group_by(Neighborhood) %>% 
          filter(Occurred.Date >= date_choice[1] & Occurred.Date <= date_choice[2] & 
                   Crime.Subcategory == crime_choice &
                   Occurred.Time >= time_choice[1] & Occurred.Time <= time_choice[2]) %>% 
          count() 
        plot_title <- paste("Crimes Rates of", crime_choice, "from", format(date_choice[1], "%B %d, %Y"), 
                            "to", format(date_choice[2], "%B %d, %Y"))
        ggplot(crimes) +
          geom_col(aes(x=Neighborhood, y=n), fill = "lightblue", color = "darkblue") +
          labs(title=plot_title, y="Number of Occurences", x="Type of Crime") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
}

shinyServer(my_server)