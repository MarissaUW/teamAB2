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
      select(Occurred.Date, Crime.Subcategory, Precinct)%>%
      mutate(Year = substr(Occurred.Date, 7, 10))%>%
      select(Crime.Subcategory, Precinct, Year)%>%
      filter(Precinct == input$precinct & Year == input$year)%>%
      group_by(Crime.Subcategory)%>%
      count()%>%
      data.frame()
      
      
      data1$Crime.Subcategory <- 
        recode(data1$Crime.Subcategory,
               "AGGRAVATED ASSAULT" = "ASSAULT",
               "AGGRAVATED ASSAULT-DV" = "ASSAULT",
               "ARSON" = "OTHER",
               "BURGLARY-COMMERCIAL" = "BURGLARY",
               "BURGLARY-COMMERCIAL-SECURE PARKING" = "BURGLARY",
               "BURGLARY-RESIDENTIAL" = "BURGLARY",
               "BURGLARY-RESIDENTIAL-SECURE PARKING" = "BURGLARY",
               "CAR PROWL" = "CAR PROWL",
               "DISORDERLY CONDUCT" = "OTHER",
               "DUI" = "DUI",
               "FAMILY OFFENSE-NONVIOLENT" = "FAMILY OFFENSE-NONVIOLENT",
               "GAMBLE" = "OTHER",
               "HOMICIDE" = "HOMICIDE",
               "LIQUOR LAW VIOLATION" = "OTHER",
               "LOITERING" = "OTHER",
               "MOTOR VEHICLE THEFT" = "THEFT",
               "NARCOTIC" = "NARCOTIC",
               "PORNOGRAPHY" = "PORNOGRAPHY",
               "PROSTITUTION" = "PROSTITUTION",
               "RAPE" = "RAPE",
               "ROBBERY-COMMERCIAL" = "ROBBERY",
               "ROBBERY-RESIDENTIAL" = "ROBBERY",
               "ROBBERY-STREET" = "ROBBERY",
               "SEX OFFENSE-OTHER" = "SEX OFFENSE-OTHER",
               "THEFT-ALL OTHER" = "THEFT",
               "THEFT-BICYCLE" = "THEFT",
               "THEFT-BUILDING" = "THEFT",
               "THEFT-SHOPLIFT" = "THEFT",
               "TRESPASS" = "TRESPASS",
               "WEAPON" = "WEAPON"
        )
    
      
      #plot
      ggplot(data1, aes(x=Crime.Subcategory, y=n) ) +
        geom_bar(stat="identity", fill="#49c9d4") +
        coord_flip()+
        xlab("Type of Crime") +
        ylab("Number of Incidents")
  })
  
  
  
  
  
  
  
  
  output$Plot2 <- renderPlot({
    
    # filter data frame into correct frame for plotting
    data2 <- readData %>% 
      select(Occurred.Date, Crime.Subcategory,Precinct)%>%
      mutate(Year = substr(Occurred.Date, 7, 10))%>%
      select(Crime.Subcategory, Precinct, Year)%>%
      filter(Precinct == input$precinct & Year == input$year)%>%
      group_by(Crime.Subcategory)%>%
      count()%>%
      data.frame()
    
   data2$Crime.Subcategory <-
      recode(data2$Crime.Subcategory,
             "AGGRAVATED ASSAULT" = "ASSAULT",
             "AGGRAVATED ASSAULT-DV" = "ASSAULT",
             "ARSON" = "OTHER",
             "BURGLARY-COMMERCIAL" = "BURGLARY",
             "BURGLARY-COMMERCIAL-SECURE PARKING" = "BURGLARY",
             "BURGLARY-RESIDENTIAL" = "BURGLARY",
             "BURGLARY-RESIDENTIAL-SECURE PARKING" = "BURGLARY",
             "CAR PROWL" = "CAR PROWL",
             "DISORDERLY CONDUCT" = "OTHER",
             "DUI" = "DUI",
             "FAMILY OFFENSE-NONVIOLENT" = "FAMILY OFFENSE-NONVIOLENT",
             "GAMBLE" = "OTHER",
             "HOMICIDE" = "OTHER",
             "LIQUOR LAW VIOLATION" = "OTHER",
             "LOITERING" = "OTHER",
             "MOTOR VEHICLE THEFT" = "THEFT",
             "NARCOTIC" = "OTHER",
             "PORNOGRAPHY" = "PROSTITUTION/RAPE/PORNOGRAPHY",
             "PROSTITUTION" = "PROSTITUTION/RAPE/PORNOGRAPHY",
             "RAPE" = "PROSTITUTION/RAPE/PORNOGRAPHY",
             "ROBBERY-COMMERCIAL" = "ROBBERY",
             "ROBBERY-RESIDENTIAL" = "ROBBERY",
             "ROBBERY-STREET" = "ROBBERY",
             "SEX OFFENSE-OTHER" = "SEX OFFENSE-OTHER",
             "THEFT-ALL OTHER" = "THEFT",
             "THEFT-BICYCLE" = "THEFT",
             "THEFT-BUILDING" = "THEFT",
             "THEFT-SHOPLIFT" = "THEFT",
             "TRESPASS" = "OTHER",
             "WEAPON" = "OTHER"
              )
   
   
   data2 <- data2%>%
     group_by(Crime.Subcategory)%>%
     summarise( sum = sum(n))
  
 
   
  
  View(data2)
    
    #plot
  #  pie(data2$n, labels = data2$Crime.Subcategory, main="Pie Chart Visualization")
   slices <- data2$sum
   lbls <- data2$Crime.Subcategory
   pct <- round(slices/sum(slices)*100)
   lbls <- paste(lbls, pct) # add percents to labels 
   lbls <- paste(lbls,"%",sep="") # ad % to labels 
   pie(slices,labels = lbls, col=rainbow(length(lbls)),
       main="")
  
    
  })
  
})

