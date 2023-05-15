

#################################################################################################################
## source "get_geo-SpatialSoils.R" function and execute it for Rwanda RAB use case
#################################################################################################################
source("~/agwise-datasourcing/dataops/datasourcing/get_geoSpatialSoils.R")

crop_geoSpatial_soiliSDA(country = "Rwanda", useCaseName = "Rwanda_RAB", Crop = "Potato", overwrite = TRUE)
crop_geoSpatial_soilGrids(country = "Rwanda", useCaseName = "Rwanda_RAB", Crop = "Potato", overwrite = TRUE)
transform_soils_data(useCaseName = "Rwanda_RAB", Crop = "Potato", resFactor=1, overwrite = TRUE)


#################################################################################################################
### extracting soil geo-spatial data for GPS locations (this is essential to get for location we have field/survey data)
#################################################################################################################
source("~/agwise/AgWise_Scripts/data_sourcing/get_geoSpatialSoils.R")
GPS_fieldData <- readRDS("~/agwise/AgWise_Data/fieldData_analytics/UseCase_Rwanda_RAB/Potato/result/compiled_potato_fieldData.RDS")
RAB_potato_soil <- extract_soil_pointdata(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", GPSdata = GPS_fieldData, AOI = FALSE)

# readRDS("~/agwise/AgWise_Data/fieldData_analytics/UseCase_Rwanda_RAB/Potato/result/compiled_fieldData.RDS")
#################################################################################################################
## extracting soil geo-spatial data for GPS locations for predictions, for AOI
#################################################################################################################
source("~/agwise/AgWise_Scripts/data_sourcing/get_geoSpatialSoils.R")
GPSdata_AOI <- readRDS("~/agwise/AgWise_Data/data_sourcing/UseCase_Rwanda_RAB/Potato/raw/AOI_GPS.RDS")
RAB_potato_soil_AOI <- extract_soil_pointdata(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", GPSdata = GPSdata_AOI, AOI = TRUE)









