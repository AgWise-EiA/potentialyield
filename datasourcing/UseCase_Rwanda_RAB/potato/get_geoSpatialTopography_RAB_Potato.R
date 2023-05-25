
#source("~/agwise/AgWise_Scripts/data_sourcing/get_geoSpatialTopography.R")
source("~/agwise-datasourcing/dataops/datasourcing/get_geoSpatialTopography.R")

#################################################################################################################
## crop the global DEM to the target country 
## TODO currently, global directory has data only for Rwanda
#################################################################################################################
crop_geoSpatial_Topography(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", overwrite = TRUE)


#################################################################################################################
## derive the DEM variables
#################################################################################################################
derive_topography_data(country = "Rwanda",useCaseName = "RAB", Crop = "Potato", overwrite = TRUE)


#################################################################################################################
### extracting topography geo-spatial data for GPS locations (this is essential to get for location we have field/survey data)
#################################################################################################################
RAB_potato_topography_trial <- extract_topography_pointdata(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", AOI = FALSE, ID = "TLID")


#################################################################################################################
## extracting topography geo-spatial data for GPS locations for predictions, for AOI
#################################################################################################################
RAB_potato_topography_AOI <- extract_topography_pointdata(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", AOI = TRUE, ID = NULL)



