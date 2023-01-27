## Data formatting for AQ data 
## J. Morina
## 12/14/2022

#Load required Packages
library(tidyquant)
library(acs)
library(leaflet)
library(tigris)
library(dplyr)

# 1. Import and Format Data ####
AQ= read.csv("DSData.csv", header=TRUE)

#seperate the censtractfips column for easy sorting
AQ.sep <-
  tidyr::separate(
    data = AQ,
    col = CensTractFips,
    sep = c(2, 5, 6),
    into = c("State", "City",NA, "Tract"),
    remove = FALSE
  ) 

# 2. Splitting data ####
## Split by pollutant
AQ.poll <-split(AQ.sep,AQ.sep$Poll)

#Split ozone by year
ozy <-AQ.poll$ozone
ozy.year <-split(ozy, format(as.Date(ozy$Date), "%Y"))
ozy.16 <-ozy.year$`2016`
ozy.19<-ozy.year$`2019`

#Split PM2.5 by year
small <- AQ.poll$pm25
small.year<-split(small, format(as.Date(small$Date), "%Y"))
small.16 <-small.year$`2016`
small.19 <-small.year$`2019`


# 3. Grouping values based on threshold values ####
## PM2.5

total.pm.16.35 <-small.16 %>%
  group_by(CensTractFips) %>%
  summarize(
    n_gt15 = sum(Prediction > 35),
    p_gt15 = n_gt15 / 366)

total.pm.19.35 <-small.19 %>%
  group_by(CensTractFips) %>%
  summarize(
    n_gt15 = sum(Prediction > 35),
    p_gt15 = n_gt15 / 366)

total.pm.16 <-small.16 %>%
  group_by(CensTractFips) %>%
  summarize(
    n_gt15 = sum(Prediction > 15),
    p_gt15 = n_gt15 / 366)

total.pm.19 <-small.19 %>%
  group_by(CensTractFips) %>%
  summarize(
    n_gt15 = sum(Prediction > 15),
    p_gt15 = n_gt15 / 365)

## Ozone






### Once data has been manipulated add it to graph using leaflet



# 4. Data Visualization ####
# Maps plotting exceedance (15 ug/m) days

va.tracts <- tracts(state = 'VA', cb=TRUE, year=2015)

##rename to GEOID and save as character, then join AQ data with map data
## 2016

names(total.pm.16)[names(total.pm.16) == 'CensTractFips'] <- 'GEOID'

total.pm.16$GEOID<-as.character(total.pm.16$GEOID)

joined <-left_join(va.tracts,total.pm.16)

## Remove IDs with no land
joined <- joined[joined$ALAND>0,]

##create palette, must use na.color to prevent it from showing on legend
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = joined$n_gt15,
  na.color = NA
)

##pop up info for when you click on the graph
popup1 <- paste0("GEOID: ", joined$GEOID, "<br>", "PM2.5 Days over 15 ug/m3 ", round(joined$n_gt15,2))

pm25.16.map <-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
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
                     title = "Days over <br> 15 ug/m3",
                     labFormat = labelFormat(suffix = " "))

saveWidget(pm25.16.map, file="PM25.VA2016.html")


# Extra: Sub-setting data for RVA ####
## This data set is not ideal for fine spatial scales such as city level, 
## better suited for state-wide analysis
rva <-AQ.sep[AQ.sep$City=="760",]

rva.oz <-rva[rva$Poll=="ozone",]
rva.25 <-rva[rva$Poll=="pm25",]
write.csv(rva.25,"rva.pm25.csv")

years.oz <-split(rva.oz, format(as.Date(rva.oz$Date), "%Y"))
years.25 <-split(rva.25, format(as.Date(rva.25$Date), "%Y"))

pm2.5for2016 <-years.25$`2016`
pm2.5for2019 <-years.25$`2019`

rva.oz.16 <-years.oz$`2016`
rva.oz.19 <-years.oz$`2019`

greater.total.pm.16 <-rva.16.pm %>%
  group_by(Tract) %>%
  summarize(
    n_gt50 = sum(Prediction > 15),
    p_gt50 = n_gt50 / 366)

greater.total.pm.19 <-rva.19.pm %>%
  group_by(Tract) %>%
  summarize(
    n_gt50 = sum(Prediction > 15),
    p_gt50 = n_gt50 / 365)

