library(shiny)
library(dplyr)
library(ggplot2)
library(shinyjs)

my_server <- function(input, output) {
  
    crime_data <- read.csv("5yr_Crime_Data.csv", stringsAsFactors = FALSE)
  
    output$neighborhoodCrimePlot <- renderPlot({
        crime_choice <- input$crime
        date_choice_crime <- input$date_crime
        dates <- as.Date(crime_data$Occurred.Date, format = "%m/%d/%Y")
        crime_data$Occurred.Date <- dates
        time_choice <- input$time_range_crime
        
        if (input$data_choice_crime == "single_crime") {
          observeEvent(input$crime, {
            enable("crime")
          })
            if (input$asc_desc == "desc") {
            crimes <- crime_data %>% 
            group_by(Neighborhood) %>% 
            filter(Occurred.Date >= date_choice_crime[1] & Occurred.Date <= date_choice_crime[2] & 
                     Crime.Subcategory == crime_choice &
                     Occurred.Time >= time_choice[1] & Occurred.Time <= time_choice[2]) %>% 
            count() %>% 
            arrange(desc(n)) %>% 
            head(input$num_neighborhoods)  
            } else {
              crimes <- crime_data %>% 
                group_by(Neighborhood) %>% 
                filter(Occurred.Date >= date_choice_crime[1] & Occurred.Date <= date_choice_crime[2] & 
                         Crime.Subcategory == crime_choice &
                         Occurred.Time >= time_choice[1] & Occurred.Time <= time_choice[2]) %>% 
                count() %>% 
                arrange(n) %>% 
                head(input$num_neighborhoods) 
            }
        plot_title <- paste("Crimes Rates of", crime_choice, "from", format(date_choice_crime[1], "%B %d, %Y"), 
                            "to", format(date_choice_crime[2], "%B %d, %Y"))
        } else {
            observeEvent(input$crime, {
              disable("crime")
            })
            if (input$asc_desc == "desc") {
            crimes <- crime_data %>% 
              group_by(Neighborhood) %>% 
              filter(Occurred.Date >= date_choice_crime[1] & Occurred.Date <= date_choice_crime[2] & 
                       Occurred.Time >= time_choice[1] & Occurred.Time <= time_choice[2]) %>% 
              count() %>% 
                arrange(desc(n)) %>% 
                head(input$num_neighborhoods)
          } else {
            crimes <- crime_data %>% 
              group_by(Neighborhood) %>% 
              filter(Occurred.Date >= date_choice_crime[1] & Occurred.Date <= date_choice_crime[2] & 
                       Occurred.Time >= time_choice[1] & Occurred.Time <= time_choice[2]) %>% 
              count() %>% 
              arrange(n) %>% 
              head(input$num_neighborhoods)
          }
          plot_title <- paste("Crimes Rates in all Neighborhoods from", format(date_choice_crime[1], "%B %d, %Y"), 
                              "to", format(date_choice_crime[2], "%B %d, %Y"))
        }
        ggplot(crimes) +
          geom_col(aes(x=Neighborhood, y=n), fill = "lightblue", color = "darkblue") +
          labs(title=plot_title, y="Number of Occurences", x="Neighborhood") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
    
    
    
    output$crimePlot <- renderPlot({
      
      neighborhood_choice <- input$neighborhood
      date_choice_neighborhood <- input$date_neighborhood
      dates <- as.Date(crime_data$Occurred.Date, format = "%m/%d/%Y")
      crime_data$Occurred.Date <- dates
      time_choice <- input$time_range_neighborhood
      
      if (input$data_choice_neighborhood == "single_neighborhood") {
        observeEvent(input$neighborhood, {
          enable("neighborhood")
        })
        crimes <- crime_data %>% 
          group_by(Crime.Subcategory) %>% 
          filter(Occurred.Date >= date_choice_neighborhood[1] & Occurred.Date <= date_choice_neighborhood[2] & Neighborhood == neighborhood_choice &
                   Occurred.Time >= time_choice[1] & Occurred.Time <= time_choice[2]) %>% 
          count() 
        plot_title <- paste("Crimes in", neighborhood_choice, "from", format(date_choice_neighborhood[1], "%B %d, %Y"), 
                            "to", format(date_choice_neighborhood[2], "%B %d, %Y"))
      } else {
        observeEvent(input$neighborhood, {
          disable("neighborhood")
        })
        crimes <- crime_data %>% 
          group_by(Crime.Subcategory) %>% 
          filter(Occurred.Date >= date_choice_neighborhood[1] & Occurred.Date <= date_choice_neighborhood[2] &
                   Occurred.Time >= time_choice[1] & Occurred.Time <= time_choice[2]) %>% 
          count() 
        plot_title <- paste("Crimes in all Neighborhoods from", format(date_choice_neighborhood[1], "%B %d, %Y"), 
                            "to", format(date_choice_neighborhood[2], "%B %d, %Y"))
      }
      ggplot(crimes) +
        geom_col(aes(x=Crime.Subcategory, y=n), fill = "lightblue", color = "darkblue") +
        labs(title=plot_title, y="Number of Occurences", x="Type of Crime") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
}

shinyServer(my_server)
