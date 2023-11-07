#directories

country <- "Rwanda"
useCaseName <- "RAB"
Crop <- "Potato"

pathIn <- "~/agwise-datasourcing/dataops/datasourcing/Data/useCase_"
pathOut <- "~/agwise-potentialyield/dataops/potentialyield/Data/useCase_"


aoidir <- paste(pathIn, country, "_", useCaseName,"/", Crop, "/Landing/AEZ", sep="")
weatherdir <- paste(pathIn, country, "_", useCaseName,"/", Crop, "/result/geo_4cropModel", sep="")
soildir<-weatherdir
outdir1 <- paste(pathOut, country, "_", useCaseName,"/", Crop, "/result/WOFOST", sep="")
outdir2 <- paste("AOI_WOFOST_yymmdd_", format(Sys.time(), "%y%m%d"), sep="_")
# outdir2 <- paste("grid_out_yymmdd_", format(Sys.time(), "%y%m%d"), sep="_") #subdirectory (with date)

#files
##aoi files
aoifile<-file.path(aoidir,"AEZ_DEM_Dissolve.shp")

#grid weather files
tminfile<-file.path(weatherdir, "temperatureMin_Season_1_PointData_AOI.RDS")
tmaxfile<-file.path(weatherdir, "temperatureMax_Season_1_PointData_AOI.RDS")
sradfile<-file.path(weatherdir, "solarRadiation_Season_1_PointData_AOI.RDS")
windfile<-file.path(weatherdir, "windSpeed_Season_1_PointData_AOI.RDS")
precfile<-file.path(weatherdir, "Rainfall_Season_1_PointData_AOI.RDS")
rhfile<-file.path(weatherdir, "relativeHumidity_Season_1_PointData_AOI.RDS")

##grid soil and DEM data
soilfile<-file.path(soildir, "SoilDEM_PointData_AOI_profile.RDS")

#variables
weathermetavars<-c("longitude", "latitude", "startingDate", "endDate", "ID", 
                   "NAME_1", "NAME_2" )
longitudevar<-"longitude"
latitudevar<-"latitude"
sdatevar<- "startingDate"
pdatevar<-"plantingDate" #not used yet (hard coded)
hdatevar<-"endDate"
idvar<-"ID" #not used yet (hard coded)

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
tsum1<-200
tsum2<-1700

#years to include
years<-2000:2022

#specify four planting (p) and harvest (h) dates (months and day of month)
phdates<-data.frame(pmonth=08, pday=c(8,15,22,30), hmonth=12, hday=c(8,15,22,30))

#shall results be mapped in leaflet (html file exported)?
map_results<-FALSE


