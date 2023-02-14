library(leaflet)
library(tidyquant)
library(acs)
library(tigris)
library(dplyr)

# Read in EPA data
AQ = read.csv("DSData.csv", header=TRUE)
AQ$Date = as.Date(AQ$Date , format = "%b-%d-%Y")
head(AQ)

# Separate the censtractfips column for easy sorting by county if desired
AQ.sep <-
  tidyr::separate(
    data = AQ,
    col = CensTractFips,
    sep = c(2, 5, 6),
    into = c("State", "City",NA, "Tract"),
    remove = FALSE) 

# Split by pollutant
AQ.poll <-split(AQ.sep,AQ.sep$Poll)

# Split PM2.5 by year
small <- AQ.poll$pm25
small.year<-split(small, format(as.Date(small$Date), "%Y"))
small.16 <-small.year$`2016`
small.19 <-small.year$`2019`

# Group values based on thresholds
# 2016
total.pm.16 <-small.16 %>%
  group_by(CensTractFips) %>%
  summarize(
    n_gt15 = sum(Prediction > 15),
    p_gt15 = n_gt15 / 366)

# 2019
total.pm.19 <-small.19 %>%
  group_by(CensTractFips) %>%
  summarize(
    n_gt15 = sum(Prediction > 15),
    p_gt15 = n_gt15 / 365)

# View data
head(total.pm.16)
head(total.pm.19)

# Load tracts from tigris
va.tracts <- tracts(state = 'VA', cb=TRUE, year=2015)

# 2016
# Rename to GEOID, save as character, then join AQ data with map data
names(total.pm.16)[names(total.pm.16) == 'CensTractFips'] <- 'GEOID'
total.pm.16$GEOID<-as.character(total.pm.16$GEOID)
joined <-left_join(va.tracts,total.pm.16)

# Remove IDs with no land
joined <- joined[joined$ALAND>0,]

# Create color palette, must use na.color to prevent it from showing on legend
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = joined$n_gt15,
  na.color = NA)

# Pop up info for when you click on census tracts
popup1 <- paste0("GEOID: ", joined$GEOID, "<br>",
                 "PM2.5 Days over 15 μg/m3 ", round(joined$n_gt15,2))

# Make leaflet map
pm25.16.map <-leaflet() %>%
  addProviderTiles("Esri.OceanBasemap") %>%
  addPolygons(data = joined, 
              fillColor = ~pal(n_gt15), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup1) %>%
  leaflet::addLegend(pal = pal, 
                     values = joined$n_gt15, 
                     position = "bottomright", 
                     title = "Days over <br> 15 μg/m3",
                     labFormat = labelFormat(suffix = " "))

# 2019
# Rename to GEOID, save as character, then join AQ data with map data
names(total.pm.19)[names(total.pm.19) == 'CensTractFips'] <- 'GEOID'
total.pm.19$GEOID<-as.character(total.pm.19$GEOID)
joined2 <-left_join(va.tracts,total.pm.19)

# Remove tracts with no land if present
joined2 <- joined2[joined2$ALAND>0,]

# Create color palette, must use na.color to prevent it from showing on legend
pal2 <- colorNumeric(
  palette = "YlGnBu",
  domain = joined2$n_gt15,
  na.color = NA)

# Pop up info for when you click on the census tracts
popup2 <- paste0("GEOID: ", joined2$GEOID, "<br>",
                 "PM2.5 Days over 15 μg/m3 ", round(joined2$n_gt15,2))

# Make leaflet map
pm25.19.map <-leaflet() %>%
  addProviderTiles("Esri.OceanBasemap") %>%
  addPolygons(data = joined2, 
              fillColor = ~pal2(n_gt15), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup1) %>%
  leaflet::addLegend(pal = pal2, 
                     values = joined2$n_gt15, 
                     position = "bottomright", 
                     title = "Days over <br> 15 μg/m3",
                     labFormat = labelFormat(suffix = " "))

# Visualize results
pm25.16.map
pm25.19.map
