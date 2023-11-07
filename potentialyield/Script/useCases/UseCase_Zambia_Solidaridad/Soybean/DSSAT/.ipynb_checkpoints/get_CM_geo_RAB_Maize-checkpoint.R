


#################################################################################################################
## source "get_rain_temp_summary.R" function and get rain and Relative Humidity summary data 
#################################################################################################################
source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialData_4CropModels.R")

#################################################################################################################
## get geo-spatial weather data 
## TODO get soil data working
#################################################################################################################

geoData <- readGeo_CM(country = "Rwanda",  useCaseName = "RAB", Crop = "Maize", AOI = FALSE,  Planting_month_date = NULL)

Rainfall <- geoData[[1]]
SolarRadiation <- geoData[[2]]
TemperatureMax <- geoData[[3]]
TemperatureMin <- geoData[[4]]


#################################################################################################################
## get geo-spatial weather data 
#################################################################################################################

