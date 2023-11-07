#directories
weatherdir<-"~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Rwanda_RAB/Potato/result/geo_4cropModel"
outdir1<-"~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Rwanda_RAB/Potato/result/WOFOST"
outdir2<-paste("trials_out_yymmdd_", format(Sys.time(), "%y%m%d"), sep="_") #with date

#files
#grid weather data
tminfile<-file.path(weatherdir, "temperatureMin_Season_1_PointData_AOI.RDS")
tmaxfile<-file.path(weatherdir, "temperatureMax_Season_1_PointData_AOI.RDS")
sradfile<-file.path(weatherdir, "solarRadiation_Season_1_PointData_AOI.RDS")
windfile<-file.path(weatherdir, "windSpeed_Season_1_PointData_AOI.RDS")
precfile<-file.path(weatherdir, "Rainfall_Season_1_PointData_AOI.RDS")
rhfile<-file.path(weatherdir, "relativeHumidity_Season_1_PointData_AOI.RDS")

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

