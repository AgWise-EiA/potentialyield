#directories
aoidir<-"~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Rwanda_RAB/Potato/Landing/AEZ"
weatherdir<-paste("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Rwanda_RAB/Potato/raw/geo_4cropModel/")
soildir<-weatherdir
yielddir<-"~/agwise/AgWise_Data/potential_yield/UseCase_Rwanda_RAB/Potato/raw/"
outdir1<-"~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Rwanda_RAB/Potato/result/WOFOST"
outdir2<-paste("trials_out_yymmdd_", format(Sys.time(), "%y%m%d"), "sitespecific_soil_phentuning",sep="_") #with date

#files
##aoi files
aoifile<-file.path(aoidir,"AEZ_DEM_Dissolve.shp")

##trial weather data
tminfile<-file.path(weatherdir, "temperatureMin_PointData_trial.RDS")
tmaxfile<-file.path(weatherdir, "temperatureMax_PointData_trial.RDS")
sradfile<-file.path(weatherdir, "solarRadiation_PointData_trial.RDS")
windfile<-file.path(weatherdir, "windSpeed_PointData_trial.RDS")
precfile<-file.path(weatherdir, "Rainfall_PointData_trial.RDS")
rhfile<-file.path(weatherdir, "relativeHumidity_PointData_trial.RDS")

##trial soil and DEM data
soilfile<-file.path(soildir, "SoilDEM_PointData_trial_profile.RDS")

##trial yield data
yieldfile<-file.path(yielddir,"compiled_fieldData.RDS")

#variables
weathermetavars<-c("longitude", "latitude", "startingDate", "endDate", "ID", 
                   "NAME_1", "NAME_2", "yearPi","yearHi","pl_j", "hv_j")
longitudevar<-"longitude"
latitudevar<-"latitude"
sdatevar<- "startingDate"
hdatevar<-"endDate"

#date formats
dformatdata<-"%Y-%m-%d" #format of inputdata
dformatmodel<-"%d_%m_%Y" #format used by wofost model
dformatworking<-"%Y-%m-%d" #format for data preparation (some functions does not work on dates, if not in a standard unambiguous format)

# weather variables
weathervars<-nn<-c("srad", "tmin", "tmax", "rh","wind", "prec") 

#country
country<-"rwanda" #to get latest gadm data level 2 for aggregation of points

#species
species<-"potato_701" #the default crop list to modify. Use wofost_crop() to list predefined crops

#soil
ss<-"ec1" #the default soil list to Wofost_soil() to list predefined soils 

#phenology (comment out to use default)
#tsum1<-200
#tsum2<-1700

#shall results be mapped in leaflet (html file exported)?
map_results<-FALSE


