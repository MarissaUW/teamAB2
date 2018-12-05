library(leaflet)
library(ggplot2)
library(shiny)
library(shinyjs)

crime_set <- as.data.frame(read.csv("5yr_Crime_Data.csv"))
crime_categories <- unique(crime_set$Crime.Subcategory)
neighborhoods <- unique(crime_set$Neighborhood)

Crime.Type <-  c("AGGRAVATED ASSAULT","AGGRAVATED ASSAULT-DV","ARSON","BURGLARY-COMMERCIAL","BURGLARY-COMMERCIAL-SECURE PARKING","BURGLARY-RESIDENTIAL","CAR PROWL","DISORDERLY CONDUCT","DUI","FAMILY OFFENSE-NONVIOLENT","GAMBLE","HOMICIDE","LIQUOR LAW VIOLATION","LOITERING","MOTOR VEHICLE THEFT","NARCOTIC","PORNOGRAPHY","PROSTITUTION","RAPE","ROBBERY-COMMERCIAL","ROBBERY-RESIDENTIAL","ROBBERY-STREET","SEX OFFENSE-OTHER","THEFT-ALL OTHER","THEFT-BICYCLE","THEFT-BUILDING","THEFT-SHOPLIFT","TRESPASS","WEAPON")

navbarPage("Seattle Crime Analysis",
           tabPanel("Summary",
                    h1("Seattle Crime Data "),
                    
                    h2("Data Set:"),
                    h4("Our project takes data from the", tags$a(href = "https://data.seattle.gov/Public-Safety/Crime-Data/4fs7-3vj5", "City of Seattle"),
                       "open data program. The data we have chosen to use is the crime data from the public safety department.
                       The dataset we are using was downloaded as a CSV file with up to date information as of November 15, 2018."),
                    
                    h2("Questions:"),
                    h4("The initial questions we set out asking that drove the direction of our project were:"),
                    h4(tags$li("What is the most common type of crime in the U-District?")),
                    h4(tags$li("What neighborhoods have the lowest and highest rates of crime?")),
                    h4(tags$li("Which neighborhood, in 2017, has the highest rate of car prowls?")),
                    h4("While these were our guiding questions, we designed our site to be interactive so that users can ask questions that interest them about neighborhoods,
                      crimes, and combinations of both based on a multitude of factors."),
                    
                    h2("General Observations:"),
                    h4("Some major points that we picked up on while analyzing this data were that Queen Anne, Capitol Hill, and Northgate have high crime rates compared to other
                      neighborhoods, 12am-3am is the time where the most crimes occur, and car prowls are by far the most occurring crime."),
                    
                    h2("Creators:"),
                    h4(tags$li("Claire Lynch")),
                    h4(tags$li("Ben Nielsen")),
                    h4(tags$li("Marissa Hermsen")),
                    h4(tags$li("Paola Vanegas"))
           ),
           
           ## A tab to view a graph of all the neighborhoods, sorted by crime.
           tabPanel("Neighborhoods by Crime",
                    sidebarLayout(
                      sidebarPanel(
                        ## Buttons to determine whether to view all the data (all neighborhoods for 
                        ## all crimes) or a single crime for all neighborhoods.          
                        radioButtons("data_choice_crime", "Data to View:",
                                     c("View All" = "all",
                                       "View Single Crime" = "single_crime")),
                        useShinyjs(),
                        ## A drop-down menu of different neighborhoods, only enabled when
                        ## the user wants to view a single crime.           
                        disabled(
                          selectInput("crime", "Crime",
                                      choices = crime_categories)
                        ),
                        ## Date range from beginning of dataset to end of dataset.          
                        dateRangeInput("date_crime", "Date Range",
                                       format = "mm/dd/yyyy",
                                       start = "01/01/2014",
                                       end = "12/31/2017",
                                       min = "01/01/2014",
                                       max = "01/01/2018",
                                       startview = "year",
                                       weekstart = 0),
                        ## A slider to choose the range of hours of the day to view,
                        ## from 0 (midnight at beginning of the day) to 
                        ## 24 (midnight at the end of the day).         
                        sliderInput("time_range_crime", "Time Range (Hours):",
                                    range(crime_set$Occurred.Time),
                                    min = 0,
                                    max = 24),
                        ## Buttons to determine whether to sort the data in
                        ## ascending or descending order.         
                        radioButtons("asc_desc", "Order of Data:",
                                     c("Sort by Lowest Rate of Crime" = "asc",
                                       "Sort by Highest Rate of Crime" = "desc")),
                        ## A slider to determine how many neighborhoods to view on the plot.        
                        sliderInput("num_neighborhoods", "Number of Neighborhoods to View",
                                    min = 1, 
                                    max = 59,
                                    59)
                      ),
                      ## Plotting the crime for different neighborhoods.         
                      mainPanel(
                        plotOutput("neighborhoodCrimePlot")
                      )
                    )
           ),
           
           ## A tab to view a graph of all the crimes, sorted by neighborhood.
           tabPanel("Crimes by Neighborhoods",
                    sidebarLayout(
                      sidebarPanel(
                        ## Buttons to determine whether to view all the data (all crimes for all 
                        ## neighborhoods) or the crimes for a single neighborhood.         
                        radioButtons("data_choice_neighborhood", "Data to View:",
                                     c("View All" = "all",
                                       "View Single Neighborhood" = "single_neighborhood")),
                        useShinyjs(),
                        ## A drop-down menu of different neighborhoods, only enabled when
                        ## the user wants to view a single neighborhood.         
                        disabled(
                          selectInput("neighborhood", "Neighborhood",
                                      choices = neighborhoods)
                        ),
                        ## Date range from beginning of dataset to end of dataset.        
                        dateRangeInput("date_neighborhood", "Date Range",
                                       format = "mm/dd/yyyy",
                                       start = "01/01/2014",
                                       end = "12/31/2017",
                                       min = "01/01/2014",
                                       max = "01/01/2018",
                                       startview = "year",
                                       weekstart = 0),
                        ## A slider to choose the range of hours of the day to view,
                        ## from 0 (midnight at beginning of the day) to 
                        ## 24 (midnight at the end of the day).
                        sliderInput("time_range_neighborhood", "Time Range (Hours)",
                                    range(crime_set$Occurred.Time),
                                    min = 0,
                                    max = 24)
                      ),
                      ## Plotting the crime plot.         
                      mainPanel(
                        plotOutput("crimePlot")
                      )
                    )
           ),
           
           # A tab to see a bar plot and a pie chart of very broad crimes by chosen precinct and year
           tabPanel("Broad Crime Count Type by Precinct and Year",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("precinct", "Precinct:", choices = unique(crime_set$Precinct)),
                        selectInput("year", "Year:", choices = c(2014,2015,2016,2017,2018))
                      ),
                      
                      # Show two plots of the generated distribution (bar and pie)
                      mainPanel(
                        plotOutput("Plot"), 
                        plotOutput("Plot2")
                      )
                    )
            ),
           
           # A tab to see a line plot of crime rates per month, with chosen year, neighborhood, and month
           tabPanel("Total Crime Counts per Month",
                    sidebarLayout(
                      sidebarPanel(
                        # Creates Radio Buttons that allow the user to select a year (between 2014 and 2018)
                        radioButtons("year_choice", "Choose a Year:", c(2014, 2015, 2016, 2017, 2018),
                                     selected = 2014),
                        # Displays the "month" widget 
                        uiOutput("month"),
                        # Displays the "neighborhood" widget 
                        uiOutput("neighborhood_select")
                      ),
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                        plotOutput("linePlot")
                      )
                    )
           ),
           
           tabPanel("Crime Map",
                    leafletOutput("mymap"),
                    h4(p("Use the side panel to change the neighborhoods displayed based on the crime rate range.",
                         align = "center")),
                    h4("Each neighborhood is differetniated by a color (green, orange, yellow, red) depending on
                        their total crime count over the 4 year span (2014-2018).", align = "center"),
                    h4(p("Color Key: Green = 0 - 1000, Yellow = 1001 - 5000, Orange = 5001 - 10000,
                         Red = 10001 - 23024", align = "center"))
           )
)

