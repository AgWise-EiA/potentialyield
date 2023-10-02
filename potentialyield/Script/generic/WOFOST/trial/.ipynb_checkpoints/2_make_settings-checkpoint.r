# paths
scrdir<-"~/agwise-potentialyield/dataops/potentialyield/Script/useCases/UseCase_Rwanda_RAB/Potato/WOFOST"
indir<-"~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Rwanda_RAB/Potato/raw/geo_4cropModel"
outdir1<-"~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Rwanda_RAB/Potato/result/WOFOST"
outdir2<-paste("trials_out_yymmdd_", format(Sys.time(), "%y%m%d"), sep="_") #with date

#species
species<-"potato_701" #use wofost_crop() to list predefined crops

# locations
locations<-paste0("Point_", 1:828)

# seasons
seasons<- 2021 #1979:2022

# weather data
weathervars<-nn<-c("srad", "tmin", "tmax", "rh","wind", "prec") 
datecol<-"MetaDVar"
dateformat<-"%d_%m_%Y"
weatherpath<-"~/agwise/AgWise_Data/potential_yield/UseCase_Rwanda_RAB/Potato/raw/geo_4cropModel"
tminfile<-file.path(weatherpath, "TemperatureMin", "TemperatureMin_4CM_trial_AgEra.RDS")
tmaxfile<-file.path(weatherpath, "TemperatureMax", "TemperatureMax_4CM_trial_AgEra.RDS")
sradfile<-file.path(weatherpath, "SolarRadiation", "SolarRadiation_4CM_trial_AgEra.RDS")
windfile<-file.path(weatherpath, "WindSpeed", "WindSpeed_4CM_trial_AgEra.RDS")
precfile<-file.path(weatherpath, "Rainfall", "Rainfall_4CM_trial_CHIRPS.RDS")
rhfile<-file.path(weatherpath, "RelativeHumidity", "RelativeHumidity_4CM_trial_AgEra.RDS")

