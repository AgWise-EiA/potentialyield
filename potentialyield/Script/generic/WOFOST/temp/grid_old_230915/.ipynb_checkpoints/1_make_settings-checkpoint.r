#directories
aoidir<-"~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Rwanda_RAB/Potato/Landing/AEZ"
weatherdir<-"~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Rwanda_RAB/Potato/raw/geo_4cropModel"
yielddir<-"~/agwise/AgWise_Data/potential_yield/UseCase_Rwanda_RAB/Potato/raw/"
outdir1<-"~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Rwanda_RAB/Potato/result/WOFOST"
outdir2<-paste("trials_out_yymmdd_", format(Sys.time(), "%y%m%d"), sep="_") #with date

#files
#aoi files
aoifile<-file.path(aoidir,"AEZ_DEM_Dissolve.shp")

#trial weather data
tminfile<-file.path(weatherdir, "TemperatureMin", "TemperatureMin_4CM_trial_AgEra.RDS")
tmaxfile<-file.path(weatherdir, "TemperatureMax", "TemperatureMax_4CM_trial_AgEra.RDS")
sradfile<-file.path(weatherdir, "SolarRadiation", "SolarRadiation_4CM_trial_AgEra.RDS")
windfile<-file.path(weatherdir, "WindSpeed", "WindSpeed_4CM_trial_AgEra.RDS")
precfile<-file.path(weatherdir, "Rainfall", "Rainfall_4CM_trial_CHIRPS.RDS")
rhfile<-file.path(weatherdir, "RelativeHumidity", "RelativeHumidity_4CM_trial_AgEra.RDS")

#trial yield data
yieldfile<-file.path(yielddir,"compiled_fieldData.RDS")

#date formats
dformatdata<-"%d_%m_%Y" #format of inputdata
dformatmodel<-"%d_%m_%Y" #format used by wofost model
dformatworking<-"%Y-%m-%d" #format for data preparation (some functions does not work on dates, if not in a standard unambiguous format)

# weather variables
weathervars<-nn<-c("srad", "tmin", "tmax", "rh","wind", "prec") 

#species
species<-"potato_701" #use wofost_crop() to list predefined crops

#soil
ss<-"ec1" #use wofost_soil() to list predefined soils

#elevation
elevation<-1500

