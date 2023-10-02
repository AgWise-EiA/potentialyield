pathIn <- paste("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName,"/", Crop, "/raw/geo_4cropModel/", sep="")
if(AOI == TRUE){
  Rainfall <- readRDS(paste(pathIn, "Rainfall_Season_", season, "_PointData_AOI.RDS", sep=""))
  SolarRadiation <- readRDS(paste(pathIn, "solarRadiation_Season_", season, "_PointData_AOI.RDS", sep=""))
  TemperatureMax <- readRDS(paste(pathIn, "temperatureMax_Season_", season, "_PointData_AOI.RDS", sep=""))
  TemperatureMin <- readRDS(paste(pathIn, "temperatureMin_Season_", season, "_PointData_AOI.RDS", sep=""))
  RelativeHum <- readRDS(paste(pathIn, "relativeHumidity_Season_", season, "_PointData_AOI.RDS", sep=""))
  Soil <- readRDS(paste(pathIn,"SoilDEM_PointData_AOI_profile.RDS", sep=""))
}else{
  Rainfall <- readRDS(paste(pathIn, "Rainfall_PointData_trial.RDS", sep=""))
  SolarRadiation <- readRDS(paste(pathIn, "solarRadiation_PointData_trial.RDS", sep=""))
  TemperatureMax <- readRDS(paste(pathIn, "temperatureMax_PointData_trial.RDS", sep=""))
  TemperatureMin <- readRDS(paste(pathIn, "temperatureMin_PointData_trial.RDS", sep=""))
  RelativeHum <- readRDS(paste(pathIn, "relativeHumidity_PointData_trial.RDS", sep=""))
  Soil <- readRDS(paste(pathIn, "SoilDEM_PointData_trial_profile.RDS", sep=""))
}
names(Soil)[names(Soil)=="lat"] <- "latitude"
names(Soil)[names(Soil)=="lon"] <- "longitude"
Soil <- na.omit(Soil)

if(AOI == TRUE){
  metaDataWeather <- as.data.frame(Rainfall[,1:6])
}else{
  metaDataWeather <- as.data.frame(Rainfall[,1:11])
}
metaData_Soil <-Soil[,c("longitude", "latitude","NAME_1","NAME_2")]


metaData <- merge(metaDataWeather,metaData_Soil)