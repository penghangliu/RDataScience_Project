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

datatable(data)

st_transform(data_merge,"+proj=longlat +datum=WGS84")%>%
  leaflet() %>% addTiles() %>%
  addPolygons(label=paste(data_merge$county," (Crime Rates=",data_merge$index_rate,")"),
              group = "All Crimess",
              color = "white", 
              dashArray = "3",
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 1.0, 
              fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", index_rate)(index_rate),
              highlightOptions = highlightOptions(color = "white", weight = 3,
                                                  bringToFront = TRUE))%>%
  addPolygons(label=paste(data_merge$county," (Crime Rates=",data_merge$violent_rate,")"),
              group = "Violent Crimes",
              color = "white", 
              dashArray = "3",
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 1.0, 
              fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", violent_rate)(violent_rate),
              highlightOptions = highlightOptions(color = "white", weight = 3,
                                                  bringToFront = TRUE))%>%
  addPolygons(label=paste(data_merge$county," (Crime Rates=",data_merge$firearm_rate,")"),
              group = "Firearm Crimes",
              color = "white", 
              dashArray = "3",
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 1.0, 
              fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", firearm_rate)(firearm_rate),
              highlightOptions = highlightOptions(color = "white", weight = 3,
                                                  bringToFront = TRUE))%>%
  addPolygons(label=paste(data_merge$county," (Crime Rates=",data_merge$property_rate,")"),
              group = "Property Crimes",
              color = "white", 
              dashArray = "3",
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 1.0, 
              fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", property_rate)(property_rate),
              highlightOptions = highlightOptions(color = "white", weight = 3,
                                                  bringToFront = TRUE))%>%
  addPolygons(label=paste(data_merge$county," (Gini Index=",data_merge$Gini_Index,")"),
              group = "Income Inequality",
              color = "white", 
              dashArray = "3",
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 1.0, 
              fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", Gini_Index)(Gini_Index),
              highlightOptions = highlightOptions(color = "white", weight = 3,
                                                  bringToFront = TRUE))%>%
  addPolygons(label=paste(data_merge$county," (Under Highschool=",data_merge$under_highschool,"%)"),
              group = "Education Attainment",
              color = "white", 
              dashArray = "3",
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 1.0, 
              fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", under_highschool)(under_highschool),
              highlightOptions = highlightOptions(color = "white", weight = 3,
                                                  bringToFront = TRUE))%>%
  addPolygons(label=paste(data_merge$county," (Unemployment Rates=",100*data_merge$unemployment,"%)"),
              group = "Unemployment Rates",
              color = "white", 
              dashArray = "3",
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 1.0, 
              fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", unemployment)(unemployment),
              highlightOptions = highlightOptions(color = "white", weight = 3,
                                                  bringToFront = TRUE))%>%
  addLayersControl(
    baseGroups = c("All Crimes", "Violent Crimes", "Firearm Crimes", "Property Crimes", "Income Inequality","Education Attainment","Unemployment Rates"),
    options = layersControlOptions(collapsed = FALSE)
  )%>%
  addMiniMap()%>%
  frameWidget(height = 600)

##########################################################

data_merge=mutate(data_merge,Gini_index=as.numeric(scale(Gini_Index)))
data_merge=mutate(data_merge,Under_highschool=as.numeric(scale(under_highschool)))
data_merge=mutate(data_merge,Unemployment=as.numeric(scale(unemployment)))

GLM <- glm(index_rate~Gini_index, data=data_merge, family="gaussian")
MRM <- lm(index_rate~Gini_index + Under_highschool + Unemployment, data = data_merge)
GAM <- gam(index_rate~Gini_index + Under_highschool + Unemployment, data = data_merge, family = "gaussian")

All <- gam(index_rate~Gini_index + Under_highschool + Unemployment, data = data_merge, family = "gaussian")
Violent <- gam(violent_rate~Gini_index + Under_highschool + Unemployment, data = data_merge, family = "gaussian")
Firearm <- gam(firearm_rate~Gini_index + Under_highschool + Unemployment, data = data_merge, family = "gaussian")
Property <- gam(property_rate~Gini_index + Under_highschool + Unemployment, data = data_merge, family = "gaussian")

summary(GLM)
summary(MRM)
summary(GAM)

xtable(GLM,
       caption="Summary for Generalized Linear Model")%>%
  print(type="html")

xtable(MRM,
       caption="Summary for Multiple Regression Model")%>%
  print(type="html")

xtable(summary(All)$p.table,
       caption="Summary for All Crimes Model")%>%
  print(type="html")

xtable(summary(Violent)$p.table,
       caption="Summary for Violent Crimes Model")%>%
  print(type="html")

xtable(summary(Firearm)$p.table,
       caption="Summary for Firearm Crimes Model")%>%
  print(type="html")

xtable(summary(Property)$p.table,
       caption="Summary for Property Crimes Model")%>%
  print(type="html")

vif(All)
datatable(AIC(INX,VIO,FIR,POR))


c1 <- c("All Crimes", "Violent Crimes", "Firearm Crimes", "Property Crimes")
c2 <- c(summary(All)$r.sq, summary(Violent)$r.sq, summary(Firearm)$r.sq, summary(Property)$r.sq)
rsq <- as.data.frame(c1,c2)

x <-c("All Crimes", "Violent Crimes", "Firearm Crimes", "Property Crimes")
y <-c(summary(All)$r.sq, summary(Violent)$r.sq, summary(Firearm)$r.sq, summary(Property)$r.sq)
x_name <- "Model"
y_name <- "R-squared"

df <- data.frame(x,y)
names(df) <- c(x_name,y_name)

xtable(df)
