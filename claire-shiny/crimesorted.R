library(dplyr)
library(ggplot2)

crime_set <- as.data.frame(read.csv("5yr_Crime_Data.csv"))
thefts <- c("THEFT-BUILDING", "THEFT-SHOPLIFT", "MOTOR VEHICLE THEFT", "THEFT-BICYCLE", "THEFT-ALL OTHER")
neighborhood <- "Magnolia"
filt <- filter(crime_set, crime_set$Neighborhood == "Magnolia")
filtered_crime <- filter(crime_set, crime_set$Neighborhood == neighborhood, crime_set$Crime.Subcategory == thefts)
##make neighborhood a choice in selectInput

bp <- ggplot(filtered_crime, aes(x=filtered_crime$Crime.Subcategory, 
                fill = as.factor(filtered_crime$Crime.Subcategory)))+ 
                geom_bar(width = 1, stat="count")
print(bp)


##pie <- bp + coord_polar("y", start=0)
##print(pie)

