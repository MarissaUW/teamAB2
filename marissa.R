library(jsonlite)
library(dplyr)
library(httr)
library(data.table)
library(purrr)
library(tidyr)
library(rgdal)
library(tmap)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(rgeos)
library(maptools)
library(sp)
library(ggmap)
library(magrittr)
library(tibble)
library(GISTools)
library(gghighlight)

saveRDS(clean_calldata, file = "clean_calldata.Rda")
load("filtered_incident.Rda")
#=====================================call data
rawincidentdata <- read.csv("/Users/marissa/Downloads/Seattle_Police_Department_911_Incident_Response.csv")
fi <- rawincidentdata[c(8,15,16,18,17,5,7,6)]
filtered_incident <- tbl_df(filtered_incident)

filtered_incident$DateTimeClear <- strptime(filtered_incident$DateTimeClear, "%m/%d/%Y %I:%M:%S %p")

filtered_incident <- filtered_incident[c(9,2,3,4,5,6,7,8)]
filtered_incident$DateTimeClear <- fi$Event.Clearance.Date
fi <- filtered_incident

fi$x <- strptime(fi$Event.Clearance.Date, format = "%m/%d/%Y %I:%M:%S %p")

fi$Year <- format(fi$DateTimeClear, "%Y")
fi$Hour <- format(fi$DateTimeClear, "%")

clean_calldata$Year <- format(clean_calldata$DateTime, "%Y")
clean_calldata$Time <- format(clean_calldata$DateTime, "%T")
clean_calldata$Month <- format(clean_calldata$DateTime, "%b")
clean_calldata$Day <- format(clean_calldata$DateTime, "%a")

filtered_incident$Year <- format(filtered_incident$DateTimeClear, "%Y")
filtered_incident$Month <- format(filtered_incident$DateTimeClear, "%b")
filtered_incident$Day <- format(filtered_incident$DateTimeClear, "%a")
filtered_incident$Time <- format(filtered_incident$DateTimeClear, "%T")

x <- strsplit(filtered_incident$Incident.Location, ",")

rawcalldata <- read.csv("/Users/marissa/Desktop/INFO201/FINAL PROJECT/data/5yr_Call_Data.csv", stringsAsFactors = FALSE, header = TRUE)

rawcalldata2017 <- rawcalldata %>% 
  filter(DateTimeClear >= as.POSIXct("2017-01-01") & DateTimeClear <= as.POSIXct("2017-12-31"))

test_calldata$x <- strptime(test_calldata$Original.Time.Queued, format = "%m/%d/%Y %I:%M:%S %p")

clean_calldata$DateTime <- strptime(clean_calldata$QDT, format = "%m/%d/%Y %I:%M:%S %p")
clean_calldata <- tbl_df(rawcalldata)
clean_calldata$Date <- NULL
clean_calldata <- clean_calldata %>% 
  filter(!Priority > "5")
clean_calldata <- clean_calldata %>% 
  filter(!Event.Clearance.Description == "RADIO BROADCAST AND CLEAR")
clean_calldata <- clean_calldata %>% 
  filter(!Event.Clearance.Description == "DUPLICATED OR CANCELLED BY RADIO")

EventCleared <- clean_calldata %>% 
  group_by(Event.Clearance.Description) %>% 
  tally()
clean_calldata <- clean_calldata[c(9,1,2,3,4,5,6,7)]

saveRDS(clean_calldata, file = "clean_calldata.rda")
fwrite(clean_calldata, "clean_calldata.csv")

c1 <- c1[7:21,]
c1 <- c1 %>% 
  filter(!Event.Clearance.Description == "EXTRA UNIT")

test_calldata <- tbl_df(test_calldata)
test_calldata$CAD.Event.Number <- NULL

c2 <- clean_calldata%>%
  group_by(Initial.Call.Type) %>% 
  tally()

f1 <- test_clearance %>% 
  filter(!Event.Clearance.Description == "ORAL WARNING GIVEN")

t2 <- test_calldata %>% 
  filter(!Priority > "5")

clean_calldata$QDT <- test$Original.Time.Queued

clean_calldata$event.cleared <- NULL

filtered_incident$lat <- filtered_incident$Incident.Location %>% 
  gsub("[()]", "", .) %>% 
  strsplit(", ") %>% 
  sapply("[", 1)

filtered_incident$long <- filtered_incident$Incident.Location %>% 
  gsub("[()]", "", .) %>% 
  strsplit(", ") %>% 
  sapply("[", 2)

#calldata <- Freq of call by initial/final type == call_init/call_fin
#=====================================crime data
rawcrimedata <- read.csv("/Users/marissa/Desktop/INFO201/REPOS/teamAB2/data/5yr_Crime_Data.csv", stringsAsFactors = FALSE, header = TRUE)

test_crimedata <- sample_n(rawcrimedata, 200)

#crimedata <- Freq of crime/neighborhood == crime_neighborhoods
#=====================================beat map data
rawbeatmap <- readOGR(dsn = "/Users/marissa/Desktop/INFO201/REPOS/teamAB2/data/SPD_Beats_WGS84", layer = "SPD_Beats_WGS84")
    #Coordinate reference systems
    ###crswgs84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#change to correct projection
rawbeatmap <- spTransform(rawbeatmap, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(rawbeatmap)
#add lat/lon grid
gg_beat <- ggplot()
gg_beat <- gg_beat + geom_polygon(data=rawbeatmap, aes(x=long, y=lat, group=group, fill=NA), color = "black", fill=NA, size=0.5)
gg_beat

#beats with centroid loc
beatmaps <- as.data.frame(rawbeatmap@data)
beats <- getSpatialPolygonsLabelPoints(rawbeatmap)
      beats <- as.data.table(beats)
      beats <- rename(beats, objectid=id)
      beats <- beats %>% 
        mutate(id = row_number())
beats_loc <- merge(x=beatmaps, y=beats, by="objectid", all.x=TRUE)
#=====================================neigborhood map data
rawhoodsmap <- readOGR(dsn = "/Users/marissa/Desktop/INFO201/REPOS/teamAB2/data/Neighborhoods/WGS84", layer = "Neighborhoods")
rawhoodsmap <- spTransform(rawhoodsmap, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(rawhoodsmap)
#add lat/lon grid
gg_hoods <- ggplot()
gg_hoods <- gg_hoods + geom_polygon(data = rawhoodsmap, aes(x=long, y=lat, group=group, fill=NA), color = "black", fill=NA, size=0.5)
gg_hoods
##=====================================Seattle basemap
basemap1 <- get_googlemap(center = c(lon = -122.335167, lat = 47.608013), zoom = 11, size = c(640, 640), scale = 2, format = c("png8"), maptype = c("roadmap"), language = "en-EN", sensor = FALSE, messaging = FALSE, urlonly = FALSE, filename = "map1", key = "AIzaSyBroQqyt7YAZC56CG3-EnRbmvCivDrh-pI")

layer_beatmap <- ggmap(basemap1)+
  geom_polygon(data=rawbeatmap, aes(x=long, y=lat, group=group, fill=NA), color = "black", fill="orangered4", size=0.8, alpha = .4)
layer_beatmap
beatID <- beats_loc$beat
###layer_beatmap2 <- ggmap(basemap1)+
  ###geom_polygon(data=rawbeatmap, aes(x=long, y=lat, group=group, fill=NA), color = "black", fill="orangered4", size=0.8, alpha = .4)


layer_hoodsmap <- ggmap(basemap1)+
  geom_polygon(data=rawhoodsmap, aes(x=long, y=lat, group=id, fill=NA), color = "black", fill="orangered4", size=0.5, alpha = .4)

layer_hoodsmap2 <- ggmap(basemap1)+
  geom_polygon(data=rawhoodsmap, aes(x=long, y=lat, group=group, fill=NA), color = "black", fill="orangered4", size=0.8, alpha = .4)+
  geom_point(data = beats_loc, aes(x=long, y=lat), color = "blue", size=1)+
  geom_label(data = beats_loc, aes(x=long, y=lat, label=beatID))

layer_incidents <- ggmap(basemap1)+
  geom_polygon(data=rawhoodsmap, aes(x=long, y=lat, group=group, fill=NA), color = "black", fill="orangered4", size=0.8, alpha = .4)+
  geom_point(data = filtered_incident, aes(x=long, y=lat), color = "blue", size=1)+

##=======================================================================================

##=======================================================================================
### match polygon coords(ID) with beat ID -> new columns
##// replace "toner" here with "terrain" or "watercolor"
##var layer = "toner";
##var map = new google.maps.Map(document.getElementById("element_id"), {
##center: new google.maps.LatLng(37.7, -122.4),
##zoom: 12,
##mapTypeId: layer,
##mapTypeControlOptions: {
##mapTypeIds: [layer]
##}
##});
##map.mapTypes.set(layer, new google.maps.StamenMapType(layer));

#data.frame(n=names(call_fin), crime=call_fin)
#as.data.frame(call_fin)
#call_fin <- as.data.frame(call_fin)


rawcrimedata$merged <- paste(rawcrimedata$Beat, rawcrimedata$Neighborhood, sep=",")