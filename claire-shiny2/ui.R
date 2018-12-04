library(shiny)
library(ggplot2)
library(plotly)

crime_set <- as.data.frame(read.csv("5yr_Crime_Data.csv"))
crime_categories <- unique(crime_set$Crime.Subcategory)
neighborhoods <- unique(crime_set$Neighborhood)
  
  shinyUI(navbarPage("Seattle Crime Analysis",
             tabPanel("Summary"
             ),
             
  
  tabPanel("Neighborhoods by Broad Crime",
    sidebarLayout(
      sidebarPanel(
        radioButtons("data_choice_crime", "Data to View:",
                     c("View All" = "all",
                       "View Single Crime" = "single_crime")),
        useShinyjs(),
        disabled(
          selectInput("crime", "Crime",
                      choices = crime_categories)
        ),
        dateRangeInput("date_crime", "Date Range",
                       format = "mm/dd/yyyy",
                       start = "01/01/2014",
                       end = "12/31/2017",
                       min = "01/01/2014",
                       max = "01/01/2018",
                       startview = "year",
                       weekstart = 0),
        sliderInput("time_range_crime", "Time Range (Hours):",
                    range(crime_set$Occurred.Time),
                    min = 0,
                    max = 24),
        radioButtons("asc_desc", "Order of Data:",
                     c("Sort by Lowest Rate of Crime" = "asc",
                       "Sort by Highest Rate of Crime" = "desc")),
        sliderInput("num_neighborhoods", "Number of Neighborhoods to View",
                    min = 1, 
                    max = 59,
                    59)
      ),
      mainPanel(
        plotOutput("neighborhoodCrimePlot")
      )
    )
  ),
  
  tabPanel("Broad Crimes by Neighborhoods",
           sidebarLayout(
             sidebarPanel(
               radioButtons("data_choice_neighborhood", "Data to View:",
                            c("View All" = "all",
                              "View Single Neighborhood" = "single_neighborhood")),
               useShinyjs(),
               disabled(
                 selectInput("neighborhood", "Neighborhood",
                             choices = neighborhoods)
               ),
               dateRangeInput("date_neighborhood", "Date Range",
                              format = "mm/dd/yyyy",
                              start = "01/01/2014",
                              end = "12/31/2017",
                              min = "01/01/2014",
                              max = "01/01/2018",
                              startview = "year",
                              weekstart = 0),
               sliderInput("time_range_neighborhood", "Time Range (Hours)",
                           range(crime_set$Occurred.Time),
                           min = 0,
                           max = 24)
             ),
             mainPanel(
               plotOutput("crimePlot")
             )
           )
  )
)
)


