library(dplyr)
library(sp)
library(maptools)
library(gstat)
library(rgdal)
library(raster)
library(tigris)
library(RColorBrewer)

us=getData('GADM', country='USA', level=2)
ny<-subset(us,NAME_1=='New York')
ny<-subset(ny,NAME_2!='Lake Ontario')
plot(ny)

CensusData <- read.csv("C:/Users/penghang/Desktop/Fall 2017/GEO 503 AW/RDataScience_Project/data/NYCensus2016.csv",stringsAsFactors = FALSE)
CrimeData <- read.csv("https://data.ny.gov/resource/vi5m-jckw.csv",stringsAsFactors = FALSE)

CrimeData <- filter(CrimeData, year==2016)
CrimeData$county[CrimeData$county=='St Lawrence']<-'Saint Lawrence'

data<-left_join(CrimeData,CensusData,by="county")
data_merge <- geo_join(ny, data, "NAME_2", "county")

names(data_merge)

spplot(data_merge["index_rate"], 
       col.regions = brewer.pal(n = 9, name = "OrRd"), 
       cuts = 8, 
       main = "Crime Rates in 2016")

spplot(data_merge["Gini_Index"], 
       col.regions = brewer.pal(n = 9, name = "OrRd"), 
       cuts = 8, 
       main = "Crime Rates in 2016")

spplot(data_merge["under_highschool"], 
       col.regions = brewer.pal(n = 9, name = "OrRd"), 
       cuts = 8, 
       main = "Crime Rates in 2016")

spplot(data_merge["unemployment"], 
       col.regions = brewer.pal(n = 9, name = "OrRd"), 
       cuts = 8, 
       main = "Crime Rates in 2016")
