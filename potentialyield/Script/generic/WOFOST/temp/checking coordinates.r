#load required packages
library(rstudioapi)
library(leaflet)
library(terra)
library(foreach)
library(doParallel)

# set wd to parent folder of present script
wd<-dirname(dirname(getActiveDocumentContext()$path))
setwd(wd)

#make settings
weatherpath<-"~/agwise/AgWise_Data/potential_yield/UseCase_Rwanda_RAB/Potato/raw/geo_4cropModel"
fname2<-file.path(weatherpath, "TemperatureMin", "TemperatureMin_4CM_AOI_02_05_AgEra.RDS")
fname3<-file.path(weatherpath, "TemperatureMin", "TemperatureMin_4CM_trial_AgEra.RDS")

#read data
#p1<-read.csv(fname1)
p2<-readRDS(fname2)
p3<-readRDS(fname3)

#prepare data
p2<-data.frame(Lon=as.numeric(p2['longitude',]), Lat=as.numeric(p2['latitude',]))
p2<-p2[complete.cases(p2),]

p3<-data.frame(Lon=as.numeric(p3['longitude',]), Lat=as.numeric(p3['latitude',]))
p3<-p3[complete.cases(p3),]

#visualize
m<-leaflet(data=p2)
m<-addProviderTiles(map=m, provider="Stamen.TerrainBackground")
#m<-addCircles(map=m, data=p1, lat=~Lat, lng=~Lon,
#              opacity=1, color="pink", weight=~ 5)
m<-addCircles(map=m, data=p2,lat=~Lat, lng=~Lon,
              opacity=1, color="darkblue", weight=~ 10)

m<-addCircles(map=m, data=p3, lat=~Lat, lng=~Lon,
              opacity=1, color="red", weight=~ 2)
m


