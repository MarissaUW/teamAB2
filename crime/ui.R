navbarPage("Seattle Crime Analysis",
           tabPanel("Summary"
           ),
           
           tabPanel("Crime Map",
                    leafletOutput("map"),
                    #overlay panel
                    absolutePanel(id = "description",
                                  class = "panel panel-default",
                                  fixed = T,
                                  draggable = T,
                                  top = 90,
                                  left = "auto",
                                  right = 20,
                                  bottom = "auto",
                                  width = "25%",
                                  height = "auto")
                    ),
           
           tabPanel("Plot",
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons("plotType", "Plot type",
                                     c("Scatter"="p", "Line"="l")
                        )
                      ),
                      mainPanel("Graph Title",
                        plotOutput("plot")
                      )
                    )
           )
)