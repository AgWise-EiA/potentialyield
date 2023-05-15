# 1. Sourcing required packages -------------------------------------------

packages_required <- c("terra", "sf", "rgl", "rgdal", "sp", "geodata", "tidyverse", "geosphere", "countrycode")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))

# 2. Read and cropped AgERA temperature (mean, min and max)  -------------------------------------------
## functions to read from "Global_GeoData/Landing/", crop and write the result in "useCaseName/Crop/raw"

#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param overwrite default is FALSE 
#'
#' @return raster files cropped from global data and the result will be written out in useCaseName/Crop/raw/Temperature/AgERA
#'
#' @examples get_geoSpatial_temperatureAgERA(country = "Rwanda", useCaseName = "Rwanda_RAB", Crop = "Potato", overwrite = TRUE)
get_geoSpatial_temperatureAgERA <- function(country, useCaseName, Crop, overwrite){

# 2.1. Prepare the output ####
  #TODO create a look up table to check use case - country names
  ## get country abbreviation to used in gdam function
  countryCC <- countrycode(country, origin = 'country.name', destination = 'iso3c')
  
  ## create a directory to store the cropped data: 
  pathOut <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", useCaseName, "/", Crop, "/raw/Temperature/AgERA", sep="")
  
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }

  ## read the relevant shape file from gdam to be used to crop the global data
  countryShp <- geodata::gadm(countryCC, level = 3, path='.')

# 2.2. Read the temperature layers and cropped ####  
## 2.2.1. Mean Temperature
listRaster_AgERA_Mean <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMean/AgEra", pattern=".nc$")
readLayers_AgERA_Mean <- terra::rast(paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMean/AgEra", listRaster_AgERA_Mean, sep="/"))
croppedLayer_AgERA_Mean <- terra::crop(readLayers_AgERA_Mean, countryShp)
  
## 2.2.2. Min Temperature ####
listRaster_AgERA_Min <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMin/AgEra", pattern=".nc$")
readLayers_AgERA_Min <- terra::rast(paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMin/AgEra", listRaster_AgERA_Min, sep="/"))
croppedLayer_AgERA_Min <- terra::crop(readLayers_AgERA_Min, countryShp)

## 2.2.3. Max Temperature ####
listRaster_AgERA_Max <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMax/AgEra", pattern=".nc$")
readLayers_AgERA_Max <- terra::rast(paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMax/AgEra", listRaster_AgERA_Max, sep="/"))
croppedLayer_AgERA_Max <- terra::crop(readLayers_AgERA_Max, countryShp)
  
# 2.3. Save results ####
terra::writeRaster(croppedLayer_AgERA_Mean, paste0(pathOut, "/AgERA_geospatial_TemperatureMean.tif", sep=""), filetype="GTiff", overwrite = overwrite)
terra::writeRaster(croppedLayer_AgERA_Min, paste0(pathOut, "/AgERA_geospatial_TemperatureMin.tif", sep=""), filetype="GTiff", overwrite = overwrite)
terra::writeRaster(croppedLayer_AgERA_Max, paste0(pathOut, "/AgERA_geospatial_TemperatureMax.tif", sep=""), filetype="GTiff", overwrite = overwrite)
}


# 3. Extract the point temperature data from GPS location -------------------------------------------
#' Title extracting the point temperature data for GPS of trial location from the raw temperature data and compute the seasonal parameters 
#'
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param GPSdata of the use case; containing also the planting date and harvesting date in format month/day/year
#' @param AOI TRUE if the GPS are for prediction for the target area, FALSE otherwise, it is used to avoid overwriting the point data from the trial locations. 
#'
#' @return a dataframe containing the gps location and the temperature parameters :
#'        tmean : Average tmean temperature between pl_Date and hv_Date
#'        tmin : Average tmin temperature between pl_Date and hv_Date 
#'        tmax : Average tmax temperature between pl_Date and hv_Date
#'        
#' @examples extact_pointdata(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", 
#' GPSdata = read.csv("~/agwise/AgWise_Data/data_sourcing/UseCase_Rwanda_RAB/Potato/raw/fieldData/aggregated_field_data.csv")

extract_Temp_pointdata <- function(country, useCaseName, Crop, GPSdata, AOI=FALSE){
  
  # 3.1. Get the GPS location and planting/harvesting dates ####
  
  
  GPSdata$x <- GPSdata$lon
  GPSdata$y <- GPSdata$lat
  
  GPSdata$plantingDate <- as.Date(GPSdata$plantingDate, "%m/%d/%Y") # Planting date in Date format
  GPSdata$harvestDate <- as.Date(GPSdata$harvestDate, "%m/%d/%Y") # Harvest date in Date format
  
  gpsUnique <- unique(GPSdata[, c("x", "y", "plantingDate", "harvestDate")])
  
  # Compute the median planting and harvest dates over the ground data in case of NA ####
  planting.med.y <- format(as.POSIXlt(median(GPSdata$plantingDate, na.rm=T)), "%Y")
  planting.med.m <- format(as.POSIXlt(median(GPSdata$plantingDate, na.rm=T)), "%m")
  planting.med.d <- format(as.POSIXlt(median(GPSdata$plantingDate, na.rm=T)), "%d")
  
  harvesting.med.y <- format(as.POSIXlt(median(GPSdata$harvestDate, na.rm=T)), "%Y")
  harvesting.med.m <- format(as.POSIXlt(median(GPSdata$harvestDate, na.rm=T)), "%m")
  harvesting.med.d <- format(as.POSIXlt(median(GPSdata$harvestDate, na.rm=T)), "%d")
  
  # 3.2. Get the temperature AgERA data for the use case####
  # Should be in ~/agwise/AgWise_Data/data_sourcing/UseCase/Crop/raw/Temperature/AgERA"
  
  pathin_AgERA <- paste("~/agwise/AgWise_Data/data_sourcing/UseCase_",country, "_", useCaseName,"/", Crop,"/raw/Temperature/AgERA", sep="")
  
  listRaster_AgERA_Mean <-list.files(path=pathin_AgERA, pattern=glob2rx("*AgERA*TemperatureMean.tif$"), full.names = T)
  listRaster_AgERA_Min <-list.files(path=pathin_AgERA, pattern=glob2rx("*AgERA*TemperatureMin.tif$"), full.names = T)
  listRaster_AgERA_Max <-list.files(path=pathin_AgERA, pattern=glob2rx("*AgERA*TemperatureMax.tif$"), full.names = T)
  
  # 3.3. Loop on all the gpsUnique to calculate the temperature parameters ####
  groundOut <- gpsUnique
  
  for(i in 1:nrow(gpsUnique)){
    
    ## 3.3.1. Extract the information for the i-th row ####
    print(paste0("Compute Tmean, Tmin and Tmax for ID ", i))
    groundi<-gpsUnique[i,]
    
    # Test for presence of planting and harvesting date
    if (is.na(groundi$plantingDate)){
      groundi$plantingDate <-as.Date(paste0(planting.med.y, '-', planting.med.m, '-',planting.med.d), "%Y-%m-%d")
    } 
    if (is.na(groundi$harvestDate)) {
      groundi$harvestDate <-as.Date(paste0(harvesting.med.y, '-', harvesting.med.m, '-',harvesting.med.d), "%Y-%m-%d")
    }
    
    ## 3.3.2. Test if the cropping season overlaps two civil year and read the temperature data ####
    yearPi <- format(as.POSIXlt(groundi$plantingDate), "%Y")
    yearHi <- format(as.POSIXlt(groundi$harvestDate), "%Y")
    
    # Case same year #
    if (yearPi == yearHi) {
      
      # Convert planting Date and harvesting in Julian Day #
      pl_j <-as.POSIXlt(groundi$plantingDate)$yday
      hv_j <-as.POSIXlt(groundi$harvestDate)$yday
      
      # Read for the corresponding year and date # 
      ## Mean Temp
      tmean <- terra::rast(listRaster_AgERA_Mean, lyrs=c(paste0(yearPi,'_',c(pl_j:hv_j))))
      
      ## Min Temp
      tmin<- terra::rast(listRaster_AgERA_Min, lyrs=c(paste0(yearPi,'_',c(pl_j:hv_j))))
      
      ## Max Temp
      tmax<- terra::rast(listRaster_AgERA_Max, lyrs=c(paste0(yearPi,'_',c(pl_j:hv_j))))
    }
    
    # Case two years #
    if (yearPi < yearHi) {
      
      # Convert planting Date and harvesting in Julian Day # 
      pl_j <-as.POSIXlt(groundi$plantingDate)$yday
      hv_j <-as.POSIXlt(groundi$harvestDate)$yday
      
      # Read for the corresponding years and date #
      # Mean Temp 
      tmean1<-terra::rast(listRaster_AgERA_Mean, lyrs=c(paste0(yearPi,'_',c(pl_j:365))))
      tmean2 <-terra::rast(listRaster_AgERA_Mean, lyrs=c(paste0(yearHi,'_',c(1:hv_j+1))))
      tmean <- c(tmean1, tmean2)
      
      # Min Temp 
      tmin1<-terra::rast(listRaster_AgERA_Min, lyrs=c(paste0(yearPi,'_',c(pl_j:365))))
      tmin2 <-terra::rast(listRaster_AgERA_Min, lyrs=c(paste0(yearHi,'_',c(1:hv_j+1))))
      tmin <- c(tmin1, tmin2)
      
      # Max Temp 
      tmax1<-terra::rast(listRaster_AgERA_Max, lyrs=c(paste0(yearPi,'_',c(pl_j:365))))
      tmax2 <-terra::rast(listRaster_AgERA_Max, lyrs=c(paste0(yearHi,'_',c(1:hv_j+1))))
      tmax <- c(tmax1, tmax2)
    }
    
    ## 3.3.3 Extract the information for the i-th row ####
    xy <- data.frame(groundi$x, groundi$y)
    tmeani<-terra::extract(tmean, xy,method='simple', cells=FALSE)
    tmini<-terra::extract(tmin, xy,method='simple', cells=FALSE)
    tmaxi<-terra::extract(tmax, xy,method='simple', cells=FALSE)
    
    ## 3.3.4. Compute the rainy season parameters ####
    # Compute the average mean temp
    meani<-mean(as.numeric(tmeani[c(2:length(tmeani))]))
    groundOut$tmean[i]<-meani
    
    # Compute the average min temp
    mini<-mean(as.numeric(tmini[c(2:length(tmini))]))
    groundOut$tmin[i]<-mini
    
    # Compute the average max temp
    maxi<-mean(as.numeric(tmaxi[c(2:length(tmaxi))]))
    groundOut$tmax[i]<-maxi
  }
  
  print("End of the loop")
  
  # 3.4 Writing of output: it will be saved in data-sourcing result and field-analytics raw. the plan is field analytics aggregates all the data ans sends one file to response-functions to be used by ML
  
  f_name <- ifelse(AOI == TRUE, "geospatial_TemperaturePointData_AOI.RDS", "geospatial_TemperaturePointData.RDS")
  
  pathOut1 <- paste("~/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_", useCaseName,"/", Crop, "/result/", sep="")
  dirName <- pathOut1
  if (!dir.exists(dirName)){
    dir.create(dirName, recursive = T)
  }
  
  saveRDS(object = groundOut, 
          file=paste(pathOut1, f_name, sep=""))
  
  
  pathOut2 <- paste("~/agwise/AgWise_Data/fieldData_analytics/UseCase_", country, "_", useCaseName,"/", Crop, "/raw/", sep="")
  dirName <- pathOut2
  if (!dir.exists(dirName)){
    dir.create(dirName, recursive = T)
  }
  
  saveRDS(object = groundOut, 
          file=paste(pathOut2, f_name, sep=""))
  
  
  print ("End of the aggregate rain function for prediction ")
}



############## Below waiting for the optimal planting dates from the yield potential module ###############

# 4. Transform AgERA temperature (mean, min and max)  for prediction -------------------------------------------
# functions to read from "useCaseName/Crop/raw" and do data processing/derived variables etc and write the result in "UseCase/Crop/transform"

#' @description function to transform soil data and generate derived variables
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param resFactor is an aggregation factor to change the resolution of the layers, soil data in global are at 1km res
#' @param overwrite default is FALSE 
#' @param pathOut path to save the result: TODO When the data architect (DA) is implemented pathOut = "usecaseName/crop/transform/soil"
#'
#' @return raster files from the useCase/Crop/raw/temperature, compute the below, average and above scenario and the results will be written out in useCaseName/Crop/transform/Temperature/AgEra
#' agwise/AgWise_Data/data_sourcing/UseCase_Rwanda_RAB/Potato/raw/Temperature/AgERA
#'
#' @examples transform_Temperature_AgERA(useCaseName = "Rwanda_RAB", Crop = "Potato", resFactor=1, overwrite = TRUE)
transform_Temperature_AgERA <- function(useCaseName, Crop, resFactor=1, overwrite = FALSE){
  
  ## create a directory to store the transformed data: with DA this will be in "usecaseName/crop/transform"
  
  pathOut <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", useCaseName, "/", Crop, "/transform/Temperature", sep="")
  
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }
  
  # 4.1. Get the planting/harvesting dates by scenario (year x season x variety) ####
  
  # Open the data
  pathIn_Optm <- paste0("/home/jovyan/agwise/AgWise_Data/potential_yield/UseCase_", useCaseName, "/", Crop, "result/DSSAT/")
  pathIn_list <- list.files(path=pathIn_Optm, pattern="Opti")

  
  # Compute the median planting and harvest dates over the ground data in case of NA ####
  planting.med.y <- format(as.POSIXlt(median(GPSdata$plantingDate, na.rm=T)), "%Y")
  planting.med.m <- format(as.POSIXlt(median(GPSdata$plantingDate, na.rm=T)), "%m")
  planting.med.d <- format(as.POSIXlt(median(GPSdata$plantingDate, na.rm=T)), "%d")
  
  harvesting.med.y <- format(as.POSIXlt(median(GPSdata$harvestDate, na.rm=T)), "%Y")
  harvesting.med.m <- format(as.POSIXlt(median(GPSdata$harvestDate, na.rm=T)), "%m")
  harvesting.med.d <- format(as.POSIXlt(median(GPSdata$harvestDate, na.rm=T)), "%d")

  # 4.2. Get the temperature AgERA data for the use case####
  # Should be in ~/agwise/AgWise_Data/data_sourcing/UseCase/Crop/raw/Temperature/AgERA"
  
  pathin_AgERA <- paste("~/agwise/AgWise_Data/data_sourcing/UseCase_",country, "_", useCaseName,"/", Crop,"/raw/Temperature/AgERA", sep="")
  
  listRaster_AgERA_Mean <-list.files(path=pathin_AgERA, pattern=glob2rx("*AgERA*TemperatureMean.tif$"), full.names = T)
  listRaster_AgERA_Min <-list.files(path=pathin_AgERA, pattern=glob2rx("*AgERA*TemperatureMin.tif$"), full.names = T)
  listRaster_AgERA_Max <-list.files(path=pathin_AgERA, pattern=glob2rx("*AgERA*TemperatureMax.tif$"), full.names = T)
  
# Write two loop per scenario of planting date (from Syia - Should be 6) and per year
  
  return(transformedLayer)


# !!!!! OLD FUNCTION!!!!! #
# 1 List all the rainfall and read them ####
listRaster<-list.files(pathInC, pattern=".nc", full.names = T)
listRaster<-listRaster[c(1:length(listRaster)-1)] # to get the last complete year and remove the ongoing one

# 2 Read Observed data and subset the specific information ####
ground<-readxl::read_excel(pathInO)
ground<-subset(ground, ground$Crop == crop & ground$Season == season & ground$agroecology == agroeco)

# 3 Convert planting date and harvest date in Julian Day ####
pl_Date<-ground$`planting date`
pl_Date<-as.Date(paste0("1981-",pl_Date), "%Y-%m-%d")
pl_Date<-as.POSIXlt(pl_Date)$yday

hv_Date<-ground$`harvest date`
hv_Date <-as.Date(paste0("1981-",hv_Date), "%Y-%m-%d")
hv_Date<-as.POSIXlt(hv_Date)$yday

# 3 Loop on all the years to calculate the parameters ####
# Initialize empty raster for the storage
tot.out<-terra::rast(listRaster[1], lyrs=1)
tot.out<-terra::crop(tot.out, shp)
tot.out[]<-'NA'
nrd.out<-tot.out
di.out<-tot.out

## 3.1 Case same year ###

if (pl_Date < hv_Date) {
  # Loop on each year
  
  for (i in 1:length(listRaster)){
    
    
    # Read raster
    readLayers<-terra::rast(listRaster[i], lyrs=c(pl_Date:hv_Date))
    
    # Crop the raster !!!!! This should be remove and done by Eduardo
    croppedLayers<-terra::crop(readLayers, shp)
    
    if (i == 1) {
      print(paste0("Compute TR, DI and NRD for year ", i))
      # Compute the total amount of rainfall
      toti<-terra::app(croppedLayers, fun='sum')
      tot.out<-toti
      
      # Compute the Daily intensity
      dii<-toti/(hv_Date-pl_Date)
      di.out<-dii
      
      # Compute the Number of rainy day
      nrdi<- croppedLayers
      nrdi[croppedLayers<thr] <- 0
      nrdi[croppedLayers>=thr]<-1
      nrdi<-terra::app(nrdi, fun='sum')
      nrd.out<- nrdi
    } else {
      print(paste0("Compute TR, DI and NRD for year ", i))
      
      # Compute the total amount of rainfall
      toti<-terra::app(croppedLayers, fun='sum')
      terra::add(tot.out)<-toti
      
      # Compute the Daily intensity
      dii<-toti/(hv_Date-pl_Date)
      terra::add(di.out)<-dii
      
      # Compute the Number of rainy day
      nrdi<- croppedLayers
      nrdi[croppedLayers<thr] <- 0
      nrdi[croppedLayers>=thr]<-1
      nrdi<-terra::app(nrdi, fun='sum')
      terra::add(nrd.out)<- nrdi
    }
  }
}

## 3.2 Case two years ###

if (pl_Date > hv_Date) {
  # Loop on each year
  
  for (i in 1:(length(listRaster)-1)){
    # Stop of the loop one the second to last year
    
    # Read raster
    readLayers1<-terra::rast(listRaster[i], lyrs=c(pl_Date:terra::nlyr(terra::rast(listRaster[i]))))
    readLayers2<-terra::rast(listRaster[i+1], lyrs=c(1:hv_Date))
    readLayers<-c(rasti1,rasti2)
    
    # Crop the raster !!!!! This should be remove and done by Eduardo
    croppedLayers<-terra::crop(readLayers, shp)
    
    if (i == 1) {
      print(paste0("Compute TR, DI and NRD for year ", i))
      # Compute the total amount of rainfall
      toti<-terra::app(croppedLayers, fun='sum')
      tot.out<-toti
      
      # Compute the Daily intensity
      dii<-toti/(pl_Date-hv_Date)
      di.out<-dii
      
      # Compute the Number of rainy day
      nrdi<- croppedLayers
      nrdi[croppedLayers<thr] <- 0
      nrdi[croppedLayers>=thr]<-1
      nrdi<-terra::app(nrdi, fun='sum')
      nrd.out<- nrdi
    } else {
      print(paste0("Compute TR, DI and NRD for year ", i))
      
      # Compute the total amount of rainfall
      toti<-terra::app(croppedLayers, fun='sum')
      terra::add(tot.out)<-toti
      
      # Compute the Daily intensity
      dii<-toti/(pl_Date-hv_Date)
      terra::add(di.out)<-dii
      
      # Compute the Number of rainy day
      nrdi<- croppedLayers
      nrdi[croppedLayers<thr] <- 0
      nrdi[croppedLayers>=thr]<-1
      nrdi<-terra::app(nrdi, fun='sum')
      terra::add(nrd.out)<- nrdi
    }
  }
}

print("Calculation of TR, DI and NDR completed")
# End of the loop for yearly parameters calculation

# 4 Calculation of the quantiles for scenarios ####
print("Calculation of the quantiles 0.25, 0.50 and 0.75 for scenarios")
tot.q<-quantile(tot.out, probs=c(0.25,0.5, 0.75))
di.q <- quantile(di.out, probs=c(0.25,0.5, 0.75))
nrd.q <-quantile(nrd.out, probs=c(0.25,0.5, 0.75))
print ("Calculation of the quantiles completed")

# 5 Writting of output ####
# Check if the directory exists

dirName<-paste0(pathOut, '/ResponseFunction/ML_Covariates')
if (!dir.exists(dirName)){
  dir.create(dirName, recursive=T)
}

names(tot.q)<-c("tr_below", "tr_normal", "tr_above")
terra::writeRaster(tot.q, paste0(dirName,"/",crop,"_",season, "_", agroeco,"_",source,"_Total_Rainfall_Scenarios.tif"), filetype="GTiff")
names(di.q) <- c("di_below", "di_normal", "di_above")
terra::writeRaster(di.q, paste0(dirName,"/",crop,"_",season, "_", agroeco,"_",source,"_Daily_Intensity_Scenarios.tif"), filetype="GTiff")
names(nrd.q) <- c("nrd_below", "nrd_normal", "nrd_above")
terra::writeRaster(nrd.q, paste0(dirName,"/",crop,"_",season, "_", agroeco,"_",source,"_Number_Of_Rainy_Days_Scenarios.tif"), filetype="GTiff")

print ("End of the aggregate rain function for prediction ")

}

    
    
    
    


