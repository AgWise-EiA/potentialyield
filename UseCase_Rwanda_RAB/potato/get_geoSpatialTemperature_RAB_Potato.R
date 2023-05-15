

#################################################################################################################
## source "get_geoSpatialTemperature.R" function and execute it for Rwanda RAB use case
#################################################################################################################
source("~/agwise/AgWise_Scripts/data_sourcing/get_geoSpatialTemperature.R")
get_geoSpatial_temperatureAgERA(country = "Rwanda", useCaseName = "Rwanda_RAB", Crop = "Potato", overwrite = TRUE)


#################################################################################################################
### extracting temperature geo-spatial data for GPS locations (this is essential to get for location we have field/survey data)
#################################################################################################################
source("~/agwise/AgWise_Scripts/data_sourcing/get_geoSpatialTemperature.R")
GPSdata <- readRDS("~/agwise/AgWise_Data/fieldData_analytics/UseCase_Rwanda_RAB/Potato/result/Rwanda_potato_fieldYield.RDS")
extract_Temp_pointdata(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", GPSdata = GPSdata, AOI = FALSE)
                 

#################################################################################################################
## point temp data for the whole area point basis: if this is to be used for crop modelers?
#################################################################################################################
source("~/agwise/AgWise_Scripts/data_sourcing/get_geoSpatialTemperature.R")
GPSdata_AOI <- readRDS("agwise/AgWise_Data/data_sourcing/UseCase_Rwanda_RAB/Potato/raw/AOI_GPS.RDS")
extract_Temp_pointdata(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", GPSdata = GPSdata_AOI, AOI = TRUE)
