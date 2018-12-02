library(shiny)

#raw data sets
rawcalldata <- read.csv("/Users/marissa/Desktop/INFO201/FINAL PROJECT/data/5yr_Call_Data.csv", stringsAsFactors = FALSE, header = TRUE)
rawcrimedata <- read.csv("/Users/marissa/Desktop/INFO201/REPOS/teamAB2/data/5yr_Crime_Data.csv", stringsAsFactors = FALSE, header = TRUE)
rawincidentdata <- read.csv("/Users/marissa/Downloads/Seattle_Police_Department_911_Incident_Response.csv", stringsAsFactors = FALSE, header = TRUE)

#test data sets
test_calldata <- sample_n(rawcalldata, 200)
test_crimedata <- sample_n(rawcrimedata, 200)
#data subssets
clean_calldata
clean_crimedata
filtered_incident

calldata_2018 <- clean_calldata




#raw beat map
rawbeatmap <- readOGR(dsn = "/Users/marissa/Desktop/INFO201/REPOS/teamAB2/data/SPD_Beats_WGS84", layer = "SPD_Beats_WGS84")
#change to correct projection
rawbeatmap <- spTransform(rawbeatmap, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#raw neighborhoods map
rawhoodsmap <- readOGR(dsn = "/Users/marissa/Desktop/INFO201/REPOS/teamAB2/data/Neighborhoods/WGS84", layer = "Neighborhoods")
#change to correct projection
rawhoodsmap <- spTransform(rawhoodsmap, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
##=====================================Seattle basemap
basemap1 <- get_googlemap(center = c(lon = -122.335167, lat = 47.608013), zoom = 11, size = c(640, 640), scale = 2, format = c("png8"), maptype = c("roadmap"), language = "en-EN", sensor = FALSE, messaging = FALSE, urlonly = FALSE, filename = "map1", key = "AIzaSyBroQqyt7YAZC56CG3-EnRbmvCivDrh-pI")
#beats overlay
layer_beatmap <- ggmap(basemap1)+
  geom_polygon(data=rawbeatmap, aes(x=long, y=lat, group=group, fill=NA), color = "black", fill="orangered4", size=0.8, alpha = .4)
#Neighborhoods overlay
layer_hoodsmap <- ggmap(basemap1)+
  geom_polygon(data=rawhoodsmap, aes(x=long, y=lat, group=id, fill=NA), color = "black", fill="orangered4", size=0.5, alpha = .4)
