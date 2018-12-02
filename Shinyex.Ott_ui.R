## chasing towers UI

ui <- fluidPage(
   titlePanel("Chasing Towers"),
   fluidRow(
      column(4,
             navlistPanel(
                "site",
                tabPanel(
                   "max distance",
                   titlePanel("distance b/w cells inside site"),
                   uiOutput("maxSiteDistanceSlider"),
                   uiOutput("siteSelector.maxDistance"),
                   textOutput("siteLocation"),
                   value = "siteDistance"
                ),
                tabPanel(
                           # districts that pop up in 06 and 07
                   "new districts",
                   titlePanel("new districts in sources:"),
                   uiOutput("sourceSelector.newDistricts"),
                   value = "newDistrictSources"
                ),
                tabPanel(
                   "enter site code",
                   uiOutput("siteSelector.manual"),
                   value = "siteManual"
                ),
                "cell",
                tabPanel(
                   "max distance",
                   titlePanel("distance b/w cells across sources"),
                   uiOutput("maxCellDistanceSlider"),
                   uiOutput("cellSelector.maxDistance"),
                   textOutput("cellLocation"),
                   value = "cellDistance"
                ),
                tabPanel(
                   "consistent site",
                   titlePanel("cell consistently belongs to the same site"),
                   uiOutput("cellSelector.siteOK"),
                   value = "cellSiteOK"
                ),
                tabPanel(
                   "enter cell code",
                   uiOutput("cellSelector.manual"),
                   value = "cellManual"
                ),
                id = "type"
             )
             ),
      column(8,
             leaflet::leafletOutput("map")
             )
   ),
   fluidRow(
      tableOutput("showData")
   )
)
