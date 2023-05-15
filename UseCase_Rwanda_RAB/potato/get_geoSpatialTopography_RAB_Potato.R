

#################################################################################################################
## source "get_geoSpatialTopography.R" function and execute it for Rwanda RAB use case
#################################################################################################################
source("~/agwise/AgWise_Scripts/data_sourcing/get_geoSpatialTopography.R")
crop_geoSpatial_Topography(country = "Rwanda", useCaseName = "Rwanda_RAB", Crop = "Potato", overwrite = TRUE)
derive_topography_data(country = "Rwanda",useCaseName = "RAB", Crop = "Potato", overwrite = TRUE)


#################################################################################################################
### extracting topography geo-spatial data for GPS locations (this is essential to get for location we have field/survey data)
#################################################################################################################
source("~/agwise/AgWise_Scripts/data_sourcing/get_geoTopography.R")
GPS_fieldData <- readRDS("~/agwise/AgWise_Data/fieldData_analytics/UseCase_Rwanda_RAB/Potato/result/compiled_potato_fieldData.RDS")
RAB_potato_topography <- extract_topography_pointdata(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", GPSdata = GPS_fieldData, AOI = FALSE)

# readRDS("~/agwise/AgWise_Data/fieldData_analytics/UseCase_Rwanda_RAB/Potato/result/compiled_fieldData.RDS")
#################################################################################################################
## extracting topography geo-spatial data for GPS locations for predictions, for AOI
#################################################################################################################
source("~/agwise/AgWise_Scripts/data_sourcing/get_geoSpatialSoils.R")
GPSdata_AOI <- readRDS("~/agwise/AgWise_Data/data_sourcing/UseCase_Rwanda_RAB/Potato/raw/AOI_GPS.RDS")
RAB_potato_topography_AOI <- extract_topography_pointdata(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", GPSdata = GPSdata_AOI, AOI = TRUE)