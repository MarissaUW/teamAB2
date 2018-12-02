library(shiny) 

shinyUI(fluidPage(

  title = "SFO City Crime Analysis",
  br(),
  img(src = "http://www.treselle.com/wp-content/uploads/2014/03/treselle-systems-logo.png", height = 100, width = 200),
  fluidRow(
    column(3,
      dateRangeInput(inputId = "date_time",  
                   label = "Choose Date Range", 
                   start = "2013-01-01",
                   end = "2014-01-01"
	 ),
     br(),
     selectInput("dayweek", "Day of Week:",
            c("All", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
     br(),
     selectInput("allcrimes", "Crime Category:",
            c("All", "OTHER OFFENSES","THEFT","NON-CRIMINAL","KIDNAPPING","ROBBERY","FRAUD","SEX","WARRANTS","ASSAULT","WEAPON LAWS","DRUGS","ARSON","TRESPASS"))
    ),    
    column(8, 
	  h3("SFO City Crime Dataset Analysis"),
      tabsetPanel( 
      tabPanel("Summary", h4(verbatimTextOutput("dim")), h4("Structure of Dataset"), verbatimTextOutput("structure"), h4("Summary of Dataset"), verbatimTextOutput("summary"), h4("Crime Categories"), verbatimTextOutput("categoryTable")),
      tabPanel("Data Table", dataTableOutput("table")),
      tabPanel("Observation",  column(6, uiOutput("obs_names"), tableOutput("summaryTable")), column(5, tableOutput("uniqueValues"))),
      tabPanel("Crime Plot",  column(6, htmlOutput("crimePlot"))),
      tabPanel("Crime HeatMap",  column(6, plotOutput("crimeHeatMap",width="750px",height="1000px"))),
      tabPanel("Crime Calendar",  column(6, htmlOutput("calendarCrimePlot")))
	  )
    )
  )
)
)