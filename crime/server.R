library(leaflet)
library(shiny)
library(dplyr)
library(ggplot2)
library(shinyjs)


function(input, output, session) {
  output$summary <- renderPrint({
    summary(cars)
  })
  
  output$map <- renderLeaflet({
        leaflet() %>% 
        addProviderTiles(providers$OpenStreetMap.BlackAndWhite, options = providerTileOptions(noWrap = TRUE)) %>%
        setView(lng = -122.335167, lat = 47.608013, zoom = 11)
        addPolygons(data = hood.sf,
                    smoothFactor = 0.75, 
                    opacity = 0, 
                    fillOpacity = 0.6,
                    stroke = FALSE,
                    fillColor = ~factpal3(hood.sf$S_HOOD))
        addCircles(radius = ~2.2*visits, popup = popup, stroke = T,
                fillColor = DarkBlue,
                fillOpacity = 0.75)
  })
  
  output$plot <- renderPlot({
    plot(cars, type=input$plotType)
  })
  
  # Determines the color of the marker based on the number of crimes committed in given neighborhood
  getColor <- function(set){
    sapply(set$n, function(n){
      if (n <= 1000){
        "green"
      } else if (n <= 5000){
        "gold"
      } else if (n <= 10000){
        "orange"
      } else {
        "red"
      }
    })
  }
  
  group_name <- function(set){
    sapply(set$n, function(n){
      if (n <= 1000){
        "<= 1000"
      } else if (n <= 5000){
        "1001 - 5000"
      } else if (n <= 10000){
        "5001 - 10000"
      } else{
        "10001 - 23024"
      }
    })
  }
  
  output$mymap <- renderLeaflet({
    crime <- read.csv("crime_data.csv", stringsAsFactors = FALSE)
    neighborhoods <- crime %>% select(Neighborhood, long, lat, n) %>% distinct()
    
    # Filter out values based on chosen values 
    leaf_map <- leaflet(neighborhoods) %>% 
      addTiles() %>% 
      addCircleMarkers(~long, ~lat, label = ~paste(Neighborhood, "(Num of Crimes:",n,")"), radius = 5,
                       color = getColor(neighborhoods), group = group_name(neighborhoods)) %>% 
      addLayersControl(overlayGroups = c("<= 1000", "1001 - 5000", "5001 - 10000", "10001 - 23014"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      setView(lng=-122.3321, lat=47.6062, zoom = 10)
  })
  
  #define plot
  output$Plot_ben <- renderPlot({
    readData <- read.csv("5yr_Crime_Data.csv")
    
    # filter data frame into correct frame for plotting
    data1 <- readData %>% 
      select(Occurred.Date, Primary.Offense.Description, Neighborhood)%>%
      mutate(Year = substr(Occurred.Date, 7, 10))%>%
      select(Primary.Offense.Description, Neighborhood, Year)%>%
      filter(Neighborhood == input$neighborhood_ben & Year == input$year)%>%
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
  
  ## A table with all of the dataset.
  crime_data <- read.csv("5yr_Crime_Data.csv", stringsAsFactors = FALSE)
   
  output$neighborhoodCrimePlot <- renderPlot({
    crime_choice <- input$crime ## Crime that user chooses to view.
    date_choice_crime <- input$date_crime ## Whether the user wants to 
                                          ## view all the data or a single
                                          ## crime.  
    dates <- as.Date(crime_data$Occurred.Date, format = "%m/%d/%Y") ## Sort
                                          ## the dates in a different format.
    crime_data$Occurred.Date <- dates ## Set the dates in the table to the new 
                                      ## formated dates.
    time_choice <- input$time_range_crime ## The time range the user selects.
    
    ## If the user chooses a single crime to view.
    if (input$data_choice_crime == "single_crime") {
      observeEvent(input$crime, {
        enable("crime") ## Enable the crime drop-down menu.
      })
      ## If the user selects descending order.
      if (input$asc_desc == "desc") {
        ## Sort all the data by user-chosen parameters, and reflect on the graph.
        crimes <- crime_data %>% 
          group_by(Neighborhood) %>% 
          filter(Occurred.Date >= date_choice_crime[1] & Occurred.Date <= date_choice_crime[2] & 
                   Crime.Subcategory == crime_choice &
                   Occurred.Time >= time_choice[1] & Occurred.Time <= time_choice[2]) %>% 
          count() %>% 
          arrange(desc(n)) %>% 
          head(input$num_neighborhoods)  
      } else { ## If the user selects ascending order.
        ## Sort all the data by user-chosen parameters, and reflect on the graph.
        crimes <- crime_data %>% 
          group_by(Neighborhood) %>% 
          filter(Occurred.Date >= date_choice_crime[1] & Occurred.Date <= date_choice_crime[2] & 
                   Crime.Subcategory == crime_choice &
                   Occurred.Time >= time_choice[1] & Occurred.Time <= time_choice[2]) %>% 
          count() %>% 
          arrange(n) %>% 
          head(input$num_neighborhoods) 
      }
      ## Title of the plot, based on the parameters.
      plot_title <- paste("Crimes Rates of", crime_choice, "from", format(date_choice_crime[1], "%B %d, %Y"), 
                          "to", format(date_choice_crime[2], "%B %d, %Y"))
    } else { ## If the user chooses to view all crime.
      observeEvent(input$crime, {
        disable("crime") ## Disable the crime drop-down menu.
      })
      ## If the user selects descending order.
      if (input$asc_desc == "desc") {
        ## Sort all the data by user-chosen parameters, and reflect on the graph.
        crimes <- crime_data %>% 
          group_by(Neighborhood) %>% 
          filter(Occurred.Date >= date_choice_crime[1] & Occurred.Date <= date_choice_crime[2] & 
                   Occurred.Time >= time_choice[1] & Occurred.Time <= time_choice[2]) %>% 
          count() %>% 
          arrange(desc(n)) %>% 
          head(input$num_neighborhoods)
      } else { ## If the user selects ascending order.
        ## Sort all the data by user-chosen parameters, and reflect on the graph.
        crimes <- crime_data %>% 
          group_by(Neighborhood) %>% 
          filter(Occurred.Date >= date_choice_crime[1] & Occurred.Date <= date_choice_crime[2] & 
                   Occurred.Time >= time_choice[1] & Occurred.Time <= time_choice[2]) %>% 
          count() %>% 
          arrange(n) %>% 
          head(input$num_neighborhoods)
      }
      ## Title of the plot, based on the parameters.
      plot_title <- paste("Crimes Rates in all Neighborhoods from", format(date_choice_crime[1], "%B %d, %Y"), 
                          "to", format(date_choice_crime[2], "%B %d, %Y"))
    }
    ## Plot the data, based on the parameters, following specified style.
    ggplot(crimes) +
      geom_col(aes(x=Neighborhood, y=n), fill = "lightblue", color = "darkblue") +
      labs(title=plot_title, y="Number of Occurences", x="Neighborhood") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$crimePlot <- renderPlot({
    
    neighborhood_choice <- input$neighborhood ## Neighborhood that user chooses to view.
    date_choice_neighborhood <- input$date_neighborhood ## Whether the user wants to 
                                                        ## view all the data or a single
                                                        ## neighborhood. 
    dates <- as.Date(crime_data$Occurred.Date, format = "%m/%d/%Y") ## Sort
                                                    ## the dates in a different format.
    crime_data$Occurred.Date <- dates ## Set the dates in the table to the new 
                                      ## formated dates.
    time_choice <- input$time_range_neighborhood ## The time range the user selects.
    
    ## If the user chooses a single neighborhood to view.
    if (input$data_choice_neighborhood == "single_neighborhood") {
      observeEvent(input$neighborhood, {
        enable("neighborhood") ## Enable the neighborhood drop-down menu.
      })
      ## Sort all the data by user-chosen parameters.
      crimes <- crime_data %>% 
        group_by(Crime.Subcategory) %>% 
        filter(Occurred.Date >= date_choice_neighborhood[1] & Occurred.Date <= date_choice_neighborhood[2] & Neighborhood == neighborhood_choice &
                 Occurred.Time >= time_choice[1] & Occurred.Time <= time_choice[2]) %>% 
        count() 
      ## Title of the plot, based on the parameters.
      plot_title <- paste("Crimes in", neighborhood_choice, "from", format(date_choice_neighborhood[1], "%B %d, %Y"), 
                          "to", format(date_choice_neighborhood[2], "%B %d, %Y"))
    } else { ## If the user chooses to view all neighborhoods.
      observeEvent(input$neighborhood, {
        disable("neighborhood") ## Disable the neighborhood drop-down menu.
      })
      ## Sort all the data by user-chosen parameters.
      crimes <- crime_data %>% 
        group_by(Crime.Subcategory) %>% 
        filter(Occurred.Date >= date_choice_neighborhood[1] & Occurred.Date <= date_choice_neighborhood[2] &
                 Occurred.Time >= time_choice[1] & Occurred.Time <= time_choice[2]) %>% 
        count() 
      ## Title of the plot, based on the parameters.
      plot_title <- paste("Crimes in all Neighborhoods from", format(date_choice_neighborhood[1], "%B %d, %Y"), 
                          "to", format(date_choice_neighborhood[2], "%B %d, %Y"))
    }
    ## Plot the data, based on the parameters, following specified style.
    ggplot(crimes) +
      geom_col(aes(x=Crime.Subcategory, y=n), fill = "lightblue", color = "darkblue") +
      labs(title=plot_title, y="Number of Occurences", x="Type of Crime") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Read in crime data file for line plot 
  crime_for_plot <- read.csv("5yr_Crime_Data.csv", stringsAsFactors = FALSE)
  
  output$neighborhood_select <- renderUI({
    selectInput("neighborhood_choice", "Select a Neighborhood", c(unique(crime_for_plot$Neighborhood)))
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
    plot_data <- crime_for_plot %>% group_by(Occurred.Date) %>% filter(Neighborhood == neighborhood, endsWith(Occurred.Date, year),
                                                                   startsWith(Occurred.Date, month)) %>% count()
    ggplot(plot_data, aes(x=Occurred.Date, y=n, group=1)) +
      geom_line()+
      geom_point() + 
      labs(title = "Crime Counts per Day", x = "Day", y = "Crime Count") + 
      theme(axis.text.x = element_text(angle = 30))
  })
}
