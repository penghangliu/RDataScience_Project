library(knitr)
library(raster)
library(rasterVis)
library(dplyr)
library(ggplot2)
# devtools::install_github("dkahle/ggmap")
library(ggmap)
library(rgdal)
library(rgeos)
library(tidyr)
library(sf)
library(leaflet)
library(DT)
library(widgetframe)

## New Packages
library(mgcv) # package for Generalized Additive Models
library(ncf) # has an easy function for correlograms
library(grid)
library(gridExtra)
library(xtable)
library(maptools)

#Download Spatial Data using raster package.
us=getData('GADM', country='USA', level=2) #Download US Boundaries
ny<-subset(us,NAME_1=='New York')          #Subset New York state
ny<-subset(ny,NAME_2!='Lake Ontario')      #Remove Ontario Lake
plot(ny)

#Reading Census in the data folder
CensusData <- read.csv("https://raw.githubusercontent.com/penghangliu/RDataScience_Project/master/data/NYCensus2016.csv",stringsAsFactors = FALSE)
#Download Crime data from New York State Gov
CrimeData <- read.csv("https://data.ny.gov/resource/vi5m-jckw.csv",stringsAsFactors = FALSE)

CrimeData <- filter(CrimeData, year==2016)
CrimeData$county[CrimeData$county=='St Lawrence']<-'Saint Lawrence'

data<-left_join(CrimeData,CensusData,by="county")
data_merge <- geo_join(ny, data, "NAME_2", "county") %>% st_as_sf()

###################################################
datatable(data)
names(ny)

data_merge

st_transform(data_merge,"+proj=longlat +datum=WGS84")%>%
  leaflet() %>% addTiles() %>%
  addPolygons(label=paste(data_merge$county," (index=",data_merge$index_rate,")"),
              group = "index",
              color = "#444444", 
              weight = 0.1, 
              smoothFactor = 0.5,
              opacity = 1.0, 
              fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", index_rate)(index_rate),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))%>%
  addMiniMap()%>%
  frameWidget(height = 600)

