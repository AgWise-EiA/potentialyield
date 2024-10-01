setwd("D:/AgWise_APSIM/RwandaData/")
#######################################################
## sourcing the data 
stn<- read.csv("coordinates_Rwanda.csv")
names(stn)<- c("Longitude", 'Latitude', "Location")
stn<-stn[1:5,]

rain<-read.csv("Rainfall.data.coordinates_Rwanda.csv")
rain$Date = seq(as.Date("1981-01-01"), as.Date("2020-12-31"), by="days")
date<- as.data.frame(rain$Date)
rain<-rain[,-1]
rain<-rain[1:5,]

max<-read.csv("Tmax.data.coordinates_Rwanda.csv")
max<-max[,-1]
max<-max[1:5,]

min<-read.csv("Tmin.data.coordinates_Rwanda.csv")
min<-min[,-1]
min<-min[1:5,]

solar<-read.csv("S.Rad.data.coordinates_Rwanda.csv")
solar<-solar[,-1]
solar<-solar[1:5,]

#########################################################
## sourcing function to create met file
setwd("D:/AgWise_APSIM/Scripts/")
source('createMetFileFunction.R')

my_list_clm<-createMetFile(rain = rain,max = max,min = min,solar = solar,stn = stn)
save(my_list_clm, file="D:/AgWise_APSIM/my_list_clm.RData")
#source soil and climate data

#Write the weather files to a working directory and Edit the weather as per location
foreach (i =1:length(my_list_clm)) %dopar% {
  #apsimx::write_apsim_met(my_list_clm[[i]], wrt.dir = "D:/AgWise_APSIM/", filename = paste0('wth_loc_',i,'.met'))}
   apsimx::write_apsim_met(my_list_clm[[i]], wrt.dir = "D:/AgWise_APSIM/project", filename = paste0('wth_loc_',i,'.met'))}

#########################################################

#Get soil data from iscric
my_list_sol <- foreach (i = 1:nrow(stn)) %dopar% {
  tryCatch(apsimx::get_worldmodeler_soil_profile(lonlat = c(stn$Longitude[i], stn$Latitude[i]))
           , error=function(err) NA)
}
save(my_list_sol, file="D:/AgWise_APSIM/my_list_sol.RData")



