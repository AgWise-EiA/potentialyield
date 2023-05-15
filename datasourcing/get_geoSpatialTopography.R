#################################################################################################################
## sourcing required packages 
#################################################################################################################
packages_required <- c("terra", "sf", "rgl", "rgdal", "sp", "geodata", "tidyverse", "geosphere", "countrycode")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))

#################################################################################################################
## functions to read from "Global_GeoData/Landing/", crop and write the result in "useCaseName/Crop/raw"
#################################################################################################################
#' the DEM layer is aggregated at 38m res and are obtained using geodata; for info on variables and units refer to
#' @param country country name
#' @param useCaseName use case name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param overwrite default is FALSE 
#'
#' @return raster files cropped from global data and the result will be written out in useCaseName/Crop/raw/Topography
#'
#' @examples crop_geoSpatial_Topography(country = "Rwanda", useCaseName = "Rwanda_RAB", Crop = "Potato", overwrite = TRUE)

crop_geoSpatial_Topography <- function(country, useCaseName, Crop, overwrite){
  
  #TODO create a look up table to check use case - country names
  ## get country abbreviation to used in gdam function
  countryCC <- countrycode(country, origin = 'country.name', destination = 'iso3c')
  
  ## create a directory to store the cropped data: 
  pathOut <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", useCaseName, "/", Crop, "/raw/Topography", sep="")
  
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }
  
  ## read the relevant shape file from gdam to be used to crop the global data
  countryShp <- geodata::gadm(countryCC, level = 3, path='.')
  
  ## read dem layers and crop
  listRaster_dem <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/Topography", pattern=".tif$")
  
  readLayers_dem <- terra::rast(paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/Topography", listRaster_dem, sep="/"))
 
  croppedLayer_dem <- terra::crop(readLayers_dem, countryShp)
 
  ## save result
  terra::writeRaster(croppedLayer_dem, paste0(pathOut, "/dem.tif", sep=""), filetype="GTiff", overwrite = overwrite)
 
  return(croppedLayer_dem)
}



#################################################################################################################
## functions to read from "useCaseName/Crop/raw" and do data processing/derived variables etc and write the result in "UseCase/Crop/transform"
#################################################################################################################

#' @description function to derive topography variables (slope, tpi and tri)
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param overwrite default is FALSE 
#' @param pathOut path to save the result: TODO When the data architect (DA) is implemented pathOut = "usecaseName/crop/feature/topography"
#' @examples derive_topography_data(useCaseName = "Rwanda_RAB", Crop = "Potato", resFactor=1, overwrite = TRUE)

derive_topography_data <- function(country, useCaseName, Crop, overwrite = FALSE){
  
  ## create a directory to store the derived data

 pathOut <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/feature/Topography", sep="")
  
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }
  
  pathIn <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/",Crop, "/raw/Topography", sep="")
 
  ## read, crop, calculate and save the raster files
  dem <- terra::rast("dem.tif")
    terra::writeRaster(dem, filename = paste0(pathOut,"/dem.tif", sep=""), filetype = "GTiff")
  slope <- terra::terrain(dem, v = 'slope', unit = 'degrees', filename = paste0(pathOut,"/slope.tif", sep=""))
  tpi <- terra::terrain(dem, v = 'TPI', filename = paste0(pathOut,"/tpi.tif", sep=""))
  tri <- terra::terrain(dem, v = 'TRI', filename = paste0(pathOut,"/tri.tif", sep=""))
}



#' Title extracting the point topography data for GPS of trial location from dem derived data 
#'
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param GPSdata 
#' @param AOI TRUE if the GPS are for prediction for the target area, FALSE otherwise, it is used to avoid overwriting the point data from the trial locations.
#'
#' @return
#' @examples extract_topography_pointdata(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", 
#' GPSdata = read.csv("~/agwise/AgWise_Data/fieldData_analytics/UseCase_Rwanda_RAB/result/aggregated_field_data.csv"))

extract_topography_pointdata <- function(country, useCaseName, Crop, GPSdata, AOI=FALSE){
  
  GPSdata$x <- GPSdata$lon
  GPSdata$y <- GPSdata$lat
  gpsPoints <- unique(GPSdata[, c("x", "y")])
  gpsPoints <- gpsPoints %>%
    mutate_if(is.character, as.numeric)
  pathin <- paste("~/agwise/AgWise_Data/data_sourcing/UseCase_",country, "_", useCaseName,"/", Crop,"/" ,"/feature/Topography", sep="")
    
  listRaster <-list.files(path=pathin, pattern=".tif$")
  topoLayer <- terra::rast(paste(pathin, listRaster, sep="/"))
  datatopo <- terra::extract(topoLayer, gpsPoints, xy = TRUE)
  colnames(gpsPoints) <- c("lon", "lat")
  topoData <- cbind(gpsPoints, datatopo)
    
    pathOut1 <- paste("~/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_", useCaseName,"/", Crop, "/result/", sep="")
  pathOut2 <- paste("~/agwise/AgWise_Data/fieldData_analytics/UseCase_", country, "_", useCaseName,"/", Crop, "/raw/Topography", sep="")
  pathOut3 <- paste("~/agwise/AgWise_Data/response_functions/UseCase_", country, "_", useCaseName,"/", Crop, "/raw/Topography", sep="")
  pathOut4 <- paste("~/agwise/AgWise_Data/potential_yield/UseCase_", country, "_", useCaseName,"/", Crop, "/raw/Topography", sep="")
    
    if (!dir.exists(pathOut1)){
    dir.create(file.path(pathOut1), recursive = TRUE)
  }
  
  if (!dir.exists(pathOut2)){
    dir.create(file.path(pathOut2), recursive = TRUE)
  }
  
  if (!dir.exists(pathOut3)){
    dir.create(file.path(pathOut3), recursive = TRUE)
  }
  
  if (!dir.exists(pathOut4)){
    dir.create(file.path(pathOut4), recursive = TRUE)
  }
    
    f_name <- ifelse(AOI == TRUE, "geospatial_topographyPointData_AOI.RDS", "geospatial_topographyPointData_trial.RDS")
  
  saveRDS(topoData, paste(pathOut1, f_name, sep="/"))
  saveRDS(topoData, paste(pathOut2, f_name, sep="/"))
  saveRDS(topoData, paste(pathOut3, f_name, sep="/"))
  saveRDS(topoData, paste(pathOut4, f_name, sep="/"))
  
  return(topoData)
}





