library(shiny)

Crime.Type <-  c("Other theft","Vehicle crime","Burglary","Violence and sexual offences","Anti-social behaviour","Public order","Shoplifting","Other crime","Criminal damage and arson","Possession of weapons","Bicycle theft","Drugs","Robbery","Theft from the person")

ui <- navbarPage("Crime in Seattle",
           
  tabPanel("Interactive map"),
      
      
      absolutePanel(top = 10, right = 10,
            sliderInput("Year", label="Dates Available from:", 2010, 2018, value=2018, step=1, sep=""),
            sliderInput("Month", "Month:", 1, 12, value=1, step=1),
            selectInput("Crimes", "Crime Type:", c(Crime.Type)),
            actionButton("Start", "Draw Map")
    )
)
   tabPanel("Ben",
            verbatimTextOutput("summary")
   )
   tabPanel("Claire",
            verbatimTextOutput("summary")
   )
   tabPanel("Paola",
            verbatimTextOutput("summary")
   )
shinyApp(ui, server)
   
