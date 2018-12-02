#!/usr/bin/env Rscript
###
### Create a shiny app for displaying the towers
### 
source(file.path(Sys.getenv("HOME"), "projects", "afbiz", "code", "conf.R"))
source(file.path(CODEDIR, "loadData.R"))
source(file.path(CODEDIR, "tools.R"))
source(file.path(CODEDIR, "classes.R"))
source(file.path(CODEDIR, "dataClasses.R"))
library(shiny)
library(dplyr)
library(leaflet)

## cell usage
cellUsage <- loadNetworkUsage(site=FALSE) %>%
   select(cell, total_count, startDate, endDate)
## cell locations   
locations <- read.delim(file.path(DATADIR, "geographic", "cell_lookup_consistency.csv.bz2"),
                        colClasses = c("callingcellid" = "character"),
                        stringsAsFactors=FALSE) %>%
                           # avoid siteName, callingcellid etc becoming
                           # a factor
   select(-cellName, -siteName, -region5) %>%
   left_join(cellUsage, by=c("callingcellid" = "cell"))
##
maxDist <- max(locations$maxDist) %>%
   round(0)
                           # distance upper limit for maxDist sliders
## compute new districts per source
tab <- NULL
districts <- NULL
                           # in the beginning we know no districts...
sources <- unique(locations$source)
sources <- sources[order(sources)]
newDistricts <- vector("list", length(sources))
names(newDistricts) <- sources
districtSource <- setNames(character(length(districtIDList)),
                           districtIDList)
                           # a vector where there is a source for each district
for(s in sources) {
   sDistricts <- locations %>%
      filter(source == s) %>%
      magrittr::extract2("distid") %>%
      unique()
   newDistricts[[s]] <- setdiff(sDistricts, districts)
   districtSource[as.character(newDistricts[[s]])] <- sub("towers", "", s)
                           # these districts arrive in this source
   districts <- union(districts, sDistricts)
}
##

server <- function(input, output) {
   output$map <- renderLeaflet({
      data <- selectedRecords()
      if(nrow(data) > 0) {
         col <- colorFactor("Set1", data$group)(data$group)
      }
      else {
         col <- "red"
      }
      leaflet() %>%
         addProviderTiles(providers[["OpenStreetMap"]], group="OSM") %>%
         addProviderTiles(providers[["Esri.WorldImagery"]], group="ESRI WorldImagery") %>%
         addProviderTiles(providers[["HERE.hybridDay"]], group="HERE hybridDay") %>%
         fitBounds(60.47208, 29.37706, 74.88945, 38.49079) %>%
         addScaleBar(position="bottomright") %>%
         clearMarkers() %>%
         addCircleMarkers(~longitude, ~latitude, color=col,
                          label = ~siteCode,
                          popup = paste0(data$callingcellid, ":", data$total_count, " calls b/w ",
                                         data$startDate, "--", data$endDate),
                          radius=3, stroke=FALSE, fillOpacity=0.8,
                          data=data) %>%
         addLayersControl(baseGroups = c("OSM", "ESRI WorldImagery"),
                          position = "topleft")
   })
   selectedRecords <- reactive({
      ## just a reactive filter for the right type of records
      recs <- switch(input$type,
                     "cellDistance" = locations %>%
                        filter(callingcellid %in% input$cellMaxDistance),
                     "cellSiteOK" = locations %>%
                        filter(callingcellid %in% input$cellSiteOK),
                     "cellManual" = locations %>%
                        filter(callingcellid %in% gsub(" ", "", input$cellManual)),
                     "siteDistance" = locations %>%
                        filter(siteCode %in% input$siteMaxDistance),
                     "siteManual" = locations %>%
                        filter(siteCode %in% (gsub(" ", "", input$siteManual) %>% toupper())),
                     "newDistrictSources" = locations %>%
                        filter(source %in% input$sourceNewDistricts) %>%
                        filter(!duplicated(siteCode),
                               distid %in% unlist(newDistricts[input$sourceNewDistricts]))
                     )
      ## compute groups: groups of cells located in a different place
      ## do this by md5 hash of long, lat:
      recs$group <- sapply(seq(length=nrow(recs)),
                           function(i) digest::digest(recs[i, c("longitude", "latitude")]))
      recs
   })
   districts <- reactive({
      selectedRecords()$distid %>% unique
   })
   suspiciousCells <- reactive({
      ## list of cells that have too large maxDistance or fail in any of the
      ## consistency checks
      if(input$type == "cellDistance") {
         md <- input$maxCellDistance
         cells <- locations %>%
            filter(maxDist > md) %>%
            mutate(cellDistrict = paste0(callingcellid, " (", distName, ")"))
      }
      else if(input$type == "cellSiteOK") {
         cells <- locations %>%
            filter(!cellSiteOK) %>%
            mutate(cellDistrict = paste0(callingcellid, " (", distName, ")"))
      }
      if(nrow(cells) > 0) {
                           # some records match the criterion
         return(setNames(cells$callingcellid, cells$cellDistrict))
      }
      else {
         cells[NULL,]
                           # return 0-row dataframe
      }
      cells
   })
   ##
   suspiciousSites <- reactive({
      md <- input$maxSiteDistance
      cat("max site discance", md, "\n")
      sites <- locations[locations$maxDist > md,]
      sites$siteDistrict <- paste0(sites$siteCode, " (",
                                  sites$distName, ")")
      sites <- tapply(seq(length=nrow(sites)), sites$provName,
                      function(i) {
         s <- setNames(sites$siteCode[i], sites$siteDistrict[i])
         s[!duplicated(s)]
      }, simplify=FALSE)
                           # list by provinces, each component is named
                           # vector of siteCodes
      sites
   })
   ##
   output$showData <- renderTable({
      ## select the data to be shown.
      selectedRecords() %>%
         select(-group)
   }, striped=TRUE, digits=3)
   ##
   output$maxCellDistanceSlider <- renderUI({
      sliderInput("maxCellDistance", "max allowed distance across sources (km)",
                  0, maxDist, 10)
   })
   output$maxSiteDistanceSlider <- renderUI({
      cat("inside maxCellDistanceSlider\n")
      sliderInput("maxSiteDistance", "max allowed distance b/w cells (km)",
                  0, maxDist, 10)
   })
   ## 
   output$siteSelector.manual <- renderUI({
      textInput("siteManual",
                "enter siteCode (spaces allowed)",
                value = "GZN 031",
                placeholder = "GZN 031"
                )
   })
   ##
   output$cellSelector.maxDistance <- renderUI({
      sCells <- suspiciousCells()
      selectInput("cellMaxDistance",
                  label=paste0("Pick a cell code (out of ",
                               length(sCells), ")"),
                  choices=sCells)
   })
   ## 
   output$cellSelector.siteOK <- renderUI({
      sCells <- suspiciousCells()
      selectInput("cellSiteOK",
                  label=paste0("Pick a cell code (out of ",
                               length(sCells), ")"),
                  choices=sCells)
   })
   ##
   output$cellSelector.manual <- renderUI({
      textInput("cellManual",
                "enter callingcellid (spaces allowed)",
                value = "41220 501 065 0401",
                placeholder = "41220 501 065 0401"
                )
   })
   ## sites are defined here -> must create the selector here
   output$siteSelector.maxDistance <- renderUI({
      sSites <- suspiciousSites()
       selectInput("siteMaxDistance",
                   label=paste0("Pick a site code (out of ",
                               length(sSites), "/",
                               sum(sapply(sSites, length)),
                               ")"),
                  choices=sSites)
   })
   ##
   location <- reactive({
      paste("districts:", paste(districts(), collapse=", "))
   })
   output$cellLocation = renderText({
      location()
   })
   output$siteLocation = renderText({
      location()
   })
   ##
   output$sourceSelector.newDistricts <- renderUI({
      src <- c("06", "07")
      checkboxGroupInput("sourceNewDistricts",
                         "select data sources",
                         choices = setNames(paste0("towers", src), src),
                         selected = "towers07")
   })
}
