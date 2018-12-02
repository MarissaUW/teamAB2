library(shiny)
library(reshape2)
library("googleVis")
library("gridExtra")
library("ggplot2")

load("processed_sfo_crime_data.rdata")
all_week_list <- list("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
all_crime_category <- list("OTHER OFFENSES","THEFT","NON-CRIMINAL","KIDNAPPING","ROBBERY","FRAUD","SEX","WARRANTS","ASSAULT","WEAPON LAWS","DRUGS","ARSON","TRESPASS")

shinyServer(function(input, output) {
    
    #Reactive data frame that will dynamically change based on date range, day of week, and crime category chosen
    crime_data <- reactive({	
    
        #subset data frame by date range
        sfo_crime_data <- sfo_crime_data[sfo_crime_data$Incident_Date %in% seq.Date(input$date_time[1], input$date_time[2], by = "days"),]
                
        #Subset the data frame based on day of week chosen
        if(!is.null(input$dayweek)){
            if(input$dayweek == "All"){
                sfo_crime_data <- subset(sfo_crime_data, DayOfWeek %in% all_week_list)
            }
            else{
                sfo_crime_data <- subset(sfo_crime_data, DayOfWeek == as.character(input$dayweek))
            }            
        }
        #Subset the data frame based on crime category chosen
        if(!is.null(input$allcrimes)){
            if(input$allcrimes == "All"){
                sfo_crime_data <- subset(sfo_crime_data, Crime_Category %in% all_crime_category)
            }
            else{
                sfo_crime_data <- subset(sfo_crime_data, Crime_Category == as.character(input$allcrimes))
            }            
        }
                
        sfo_crime_data
    })
    
    ############# SUMMARY SECTION ###############
    #This will print the number of rows and columns in the data frame
    output$dim <- renderPrint({
        crime_data_stats <- paste("There are", nrow(crime_data()), "rows and", ncol(crime_data()), "columns for sfo_crime_data.")
        print(crime_data_stats)
    })

    #This will print the structure of the data frame, fields and its data type
    output$structure <- renderPrint({
        str(crime_data())
    })

    #This will print the summary of the data frame
    output$summary <- renderPrint({
        summary(crime_data())
    })
    
    #This will print the different crime categories and their count
    output$categoryTable <- renderPrint({
        table(crime_data()$Crime_Category)
    })    
    
    ############# DATA TABLE SECTION ###############
    #This will show JQuery data table with multiple filtering options (global and per column search)
    output$table <- renderDataTable({
        crime_data()
    }, options = list(sScrollX = "100%", bFilter=1), searchDelay = 500)
    
    ############ OBSERVATION SECTION ###############
    #Renders the observation table with number of unique values for each field in the data frame
    output$uniqueValues <- renderTable({
        melt(sapply(crime_data(), function(x) length(unique(x))), , variable.name="Columns", value.name="Unique Values")
    })
	  
    #Renders the dropdown for each field in the data frame
	output$obs_names <- renderUI({    
        observationList <- list()
        #Loop thru all field names in the data frame and choose only character and factorial variables to find observations.
        for(name in names(crime_data())){     
            print(paste("Class Type", class(crime_data()[[name]])))
            if(!is.null(crime_data()[[name]]) && ((class(crime_data()[[name]]) == "character") || class(crime_data()[[name]]) == "factor")){
                observationList[[length(observationList)+1]] <- as.character(name)
            }
        }
        
        #Dynamically create dropdown box
        selectInput("observation", 
            label = "Choose a variable to Observe",
            choices = observationList)
    })
   
      
    # Renders table based on observation selected
    output$summaryTable <- renderTable({ 
        if(input$observation %in% colnames(crime_data()) && class(input$observation) == "character"){      
          melt(table(crime_data()[, c(input$observation)]), variable.name="", value.name="# of Observation")      
        }
    })
    
    ############ CRIME PLOT SECTION ###################################
    #Combine multiple googlevis column chart and merge them
    output$crimePlot <- renderGvis({
        #Aggregate crime category to find the count for different crime categories
        crime_count <- aggregate(crime_data()$Crime_Category, by = list(crime_data()$Crime_Category), FUN = length)
        names(crime_count) <- c("Crime_Category", "Count")

        crime_count_plot <- gvisColumnChart(data = crime_count, xvar="Crime_Category", yvar="Count",
                     options=list(title="# of Crimes", 
                                  chartArea="{left:50,top:50,width: '75%', height: '75%'}",  hAxis="{textPosition: 'out'}", vAxis= "{textPosition: 'out'}", width=800, height=350))
        
        #Aggregate crime category by different time in a day
        crime_count_time_tag <- aggregate(crime_data()$Crime_Category, by = list(crime_data()$Incident_Time_Tag), FUN = length)
        names(crime_count_time_tag) <- c("Incident Time", "Count")

        crime_count_time_tag_plot <- gvisColumnChart(data = crime_count_time_tag, xvar="Incident Time", yvar=("Count"),
                     options=list(title="# of Crimes By Time", 
                                  chartArea="{left:50,top:50,width: '75%', height: '75%'}",  hAxis="{textPosition: 'out'}", vAxis= "{textPosition: 'out'}", width=600, height=300))
        
        #Aggregate crime category by different days in a week
        crime_count_dayofweek <- aggregate(crime_data()$Crime_Category, by = list(crime_data()$DayOfWeek), FUN = length)
        names(crime_count_dayofweek) <- c("Day of Week", "Count")

        crime_count_dayofweek_plot <- gvisColumnChart(data = crime_count_dayofweek, xvar="Day of Week", yvar=("Count"),
                     options=list(title="# of Crimes By Day of Week", 
                                  chartArea="{left:50,top:50,width: '75%', height: '75%'}",  hAxis="{textPosition: 'out'}", vAxis= "{textPosition: 'out'}", width=600, height=300))
        
        #Aggregate crime category by different months in a year
        crime_count_monthofyear <- aggregate(crime_data()$Crime_Category, by = list(crime_data()$Incident_Month), FUN = length)
        names(crime_count_monthofyear) <- c("Incident Month", "Count")

        crime_count_monthofyear_plot <- gvisColumnChart(data = crime_count_monthofyear, xvar="Incident Month", yvar=("Count"),
                     options=list(title="# of Crimes By Day of Week", 
                                  chartArea="{left:50,top:50,width: '75%', height: '75%'}",  hAxis="{textPosition: 'out'}", vAxis= "{textPosition: 'out'}", width=600, height=300)) 
        
        #Merge multiple column chart together
        p <- Reduce(gvisMerge, list(crime_count_plot, crime_count_time_tag_plot, crime_count_dayofweek_plot, crime_count_monthofyear_plot))
    })
    
    ############ CRIME HEAT MAP SECTION ###################################
    #Combine multiple heat map chart and merge them
    output$crimeHeatMap <- renderPlot({
        base_size <- 10
        #Aggregate crime category by different time in a day
        incidents_by_time <- aggregate(crime_data()$Crime_Category, by = list(crime_data()$Crime_Category, crime_data()$Incident_Time_Tag), FUN = length) 
        names(incidents_by_time) <- c("Crime_Category", "Incident_Time_Tag", "Count")
                
        p1 <- ggplot(incidents_by_time, aes(x= Crime_Category, y= factor(Incident_Time_Tag))) +
        geom_tile(aes(fill= Count)) + scale_x_discrete("Crimes", expand = c(0, 0)) +
        scale_y_discrete("Time of day", expand = c(0,-2)) +
        scale_fill_gradient("Number of crimes", low = "skyblue", high = "steelblue") +
        theme_grey(base_size = base_size) + ggtitle("Crimes by time of day") +
        opts(legend.position = "none", axis.ticks = theme_blank(), axis.text.x = theme_text(size = base_size, angle = 300, hjust = 0, colour = "grey50"))+
        theme(panel.grid.major = element_line(colour = NA), axis.text = element_text(size = 10), panel.grid.minor = element_line(colour = NA))
        
        #Aggregate crime category by different days in a week
        incidents_by_dayofweek <- aggregate(crime_data()$Crime_Category, by = list(crime_data()$Crime_Category, crime_data()$DayOfWeek), FUN = length)
        names(incidents_by_dayofweek) <- c("Crime_Category", "DayOfWeek", "Count")
        
        p2 <- ggplot(incidents_by_dayofweek, aes(x= Crime_Category, y= factor(DayOfWeek))) +
        geom_tile(aes(fill= Count)) + scale_x_discrete("Crimes", expand = c(0, 0)) +
        scale_y_discrete("Day of Week", expand = c(0,-2)) +
        scale_fill_gradient("Number of crimes", low = "skyblue", high = "steelblue") +
        theme_grey(base_size = base_size) + ggtitle("Crimes by day of week") +
        opts(legend.position = "none", axis.ticks = theme_blank(), axis.text.x = theme_text(size = base_size, angle = 300, hjust = 0, colour = "grey50"))+
        theme(panel.grid.major = element_line(colour = NA), axis.text = element_text(size = 10), panel.grid.minor = element_line(colour = NA))
        
        #Aggregate crime category by different months in a year
        incidents_by_monthofyear <- aggregate(crime_data()$Crime_Category, by = list(crime_data()$Crime_Category, crime_data()$Incident_Month), FUN = length)
        names(incidents_by_monthofyear) <- c("Crime_Category", "Incident_Month", "Count")
        p3 <- ggplot(incidents_by_monthofyear, aes(x= Crime_Category, y= factor(Incident_Month))) +
        geom_tile(aes(fill= Count)) + scale_x_discrete("Crimes", expand = c(0, 0)) +
        scale_y_discrete("Month of Year", expand = c(0,-2)) +
        scale_fill_gradient("Number of crimes", low = "skyblue", high = "steelblue") +
        theme_grey(base_size = base_size) + ggtitle("Crimes by month of year") +
        opts(legend.position = "none", axis.ticks = theme_blank(), axis.text.x = theme_text(size = base_size, angle = 300, hjust = 0, colour = "grey50"))+
        theme(panel.grid.major = element_line(colour = NA), axis.text = element_text(size = 10), panel.grid.minor = element_line(colour = NA))
        
        grid.arrange(p1, p2, p3)
    })
    
    ############ CRIME CALENDAR SECTION ###################################
    #Loop thru different crime categories and aggregate to find number of crimes incidents by each day in a calendar year
    output$calendarCrimePlot <- renderGvis({
        plotList <- list()
        crimeList <- sort(unique(as.character(crime_data()[, "Crime_Category"])))
        
        for(crime in crimeList){
            #subset the data frame by each crime category
            crime_data_subset <- subset(crime_data(), Crime_Category == as.character(crime))
            #Find the number of incidents happened by each day by crime
            incidents_by_day <- aggregate(crime_data_subset$Crime_Category, by = list(crime_data_subset$Incident_Date), FUN = length)
            names(incidents_by_day) <- c("Incident_Date", "Count")
            #Store all the plots in a list to merge them with Reduce
            plotList[[length(plotList)+1]] <- gvisCalendar(data=incidents_by_day, datevar="Incident_Date", numvar="Count",
                    options=list(width=800, height=180, title=paste(crime, "Incidents", sep=" "), calendar="{cellSize:12, yearLabel:{fontSize:20, color:'#444444'}, focusedCellColor:{stroke:'red'}}"))  
        }
        
        p <- Reduce(gvisMerge, plotList)        
    })
})