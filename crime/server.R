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
  
  output$summary <- renderText({
    h1("Seattle Crime Data ")
    h3("Data Set:")
    p("Our project takes data from the City of Seattle open data program. The data we have chosen to use is the crime data from the public safety department. 
      The dataset we are using was downloaded as a CSV file with up to date information as of November 15, 2018.")
    
    h3("Impact:")
    p("Our goal with this information is to present the data in a way that makes it easy for anyone, and specifically Seattle Residents, to visualize and 
      understand crime data in the greater Seattle area. We hope that this platform will help users understand the overall safety and crime incidents of 
      neighborhoods so that they can make more informed decisions on when and where they are in regards to personal safety.")
    
    h3("Questions:")
    p("The initial questions we set out asking that drove the direction of our project were:")
    p("What is the most common type of crime in the U-District?")
    p("What neighborhoods have the lowest and highest rates of crime?")
    p("Which areas have had the highest increase in crime over the past year?")
    p("While these were our guiding questions, we designed our site to be interactive so that users can ask questions that interest them about neighborhoods,
            crimes, and combinations of both based on a multitude of factors.")
    
    h3("General Observations:")
    p("Some major points that we picked up on while analyzing this data were that Queen Anne, Capitol Hill, and Northgate have high crime rates compared to other
      neighborhoods, 12am-3am is the time where the most crimes occur, and car prowls are by far the most occurring crime.")
    
  })
  
}
