
#################################################################################################################
## sourcing required packages 
#################################################################################################################
packages_required <- c("doParallel", "foreach", "chirps", "tidyverse", "dplyr", "lubridate", "stringr")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))


#################################################################################################################
## functions
#################################################################################################################
#' @description a function to crop the SolarRadiation global layer but this does duplicate a large volume of data , it is better directly to source from the global data
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param dataSource is one of c("chirts", "AgEra")
#' @param overwrite default is FALSE 
#'
#' @return raster files cropped from global data and the result will be written out in useCaseName/Crop/raw/soil/iSDA
#'
#' @examples get_geoSpatial_soiliSDA(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", dataSource = "chirts", overwrite = TRUE)
crop_geoSpatial_SolarRadiation <- function(country, useCaseName, Crop, dataSource, overwrite){
  
  #TODO create a look up table to check use case - country names
  ## get country abbreviation to used in gdam function
  countryCC <- countrycode(country, origin = 'country.name', destination = 'iso3c')
  
  ## create a directory to store the cropped data: 
  if(dataSource == "chirts"){
    pathOut <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/SolarRadiation/chirts", sep="")
    ## read soil layers and crop
    listRaster_SR <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/SolarRadiation/chirts", pattern=".nc$")
    readLayers_SR <- terra::rast(paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/SolarRadiation/chirts", listRaster_SR, sep="/"))
    fileName <- "/chirts_geospatial_SolarRadiation.tif"
  }else{
    pathOut <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/SolarRadiation/AgEra", sep="")
    listRaster_SR <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/SolarRadiation/AgEra", pattern=".nc$")
    readLayers_SR <- terra::rast(paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/SolarRadiation/AgEra", listRaster_SR, sep="/"))
    fileName <- "/AgEra_geospatial_SolarRadiation.tif"
  }
  
 
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }
  
  ## read the relevant shape file from gdam to be used to crop the global data
  countryShp <- geodata::gadm(countryCC, level = 3, path='.')
  
  ## crop the layers 
  croppedLayer_SR <- terra::crop(readLayers_SR, countryShp)
  
   ## save result
  terra::writeRaster(croppedLayer_SR, paste(pathOut, fileName , sep="/"), filetype="GTiff", overwrite = overwrite)
  
  return(croppedLayer_rf)
}





#################################################################################################################
#################################################################################################################
#' @description is a function to get average Solar Radiation during the growing season, monthly Solar Radiation
#' @param rastLayer the .nc file for the planting year, within get_rf_SR_pointdata function, this is provided by the function 
#' @param gpsdata a data frame with longitude and latitude 
#' @param pl_j the planting date as the date of the year
#' @param hv_j the harvest date as the date of the year
#'
#' @return  a data frame with total Solar Radiation, number of rainy days and monthly Solar Radiation
#' @example summary_pointdata_SolarRadiation(rastLayer1="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/SolarRadiation/AgEra/1992.nc",
                   #' rastLayer2=NULL, gpsdata=data.frame(longitude = c(29.375, 30.125), latitude = c(-2.825, -2.425)),  pl_j=35, hv_j=128,
                   #' planting_harvest_sameYear = TRUE)
summary_pointdata_SR <- function(rastLayer1= NULL, rastLayer2=NULL, gpsdata, pl_j, hv_j, planting_harvest_sameYear){
  if(planting_harvest_sameYear == TRUE){
    PlHvD <- terra::rast(rastLayer1, lyrs=c(pl_j:hv_j))
  }else{
    tmxi1 <- terra::rast(rastLayer1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastLayer1))))
    tmxi2 <- terra::rast(rastLayer2, lyrs=c(1:hv_j))
    PlHvD <- c(tmxi1, tmxi2)
  }
    
    xy <- gpsdata[, c("longitude", "latitude")]
    SRi <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
    SRi <- SRi[,-1]
    
    # Compute the total  and monthly average SR 

      tmxiq <- t(SRi[c(1:length(SRi))])
      gpsdata$AvSolarRadiation <- NULL
      for(m in 1:nrow(gpsdata)){
        gdata <- SRi[m, ]
        gpsdata$SolarRadiation[m] <- as.numeric(mean(as.numeric(gdata)))
        mdiv <- unique(c(seq(1, length(gdata), 30), length(gdata)))
        
        mrf <- c()
        for (k in 1:(length(mdiv)-1)) {
          if(k == 1){
            mrf <- c(mrf, mean(as.numeric(gdata[c(mdiv[k]:mdiv[k+1])])))
          }else{
            mrf <- c(mrf, mean(as.numeric(gdata[c((mdiv[k]+1):(mdiv[k+1]))])))
          }
        }
        
        
        if(length(mrf) > 6){## if the crop is > 6 month on the field
          mrf <- c(mrf, rep("NA", 6 - length(mrf)))
        }
        
        mrf_names <- c(paste0("solarRad_month", c(1:6)))
        for (h in 1:length(mrf_names)) {
          colname <- mrf_names[h]
          gpsdata[[colname]][m] <- mrf[h]
        }
        
      } 

      if(planting_harvest_sameYear == TRUE){
        gpsdata$plantingYear <- str_extract(rastLayer1, "[[:digit:]]+")
        gpsdata$harvestYear <-  gpsdata$plantingYear
      }else{
        gpsdata$plantingYear <- str_extract(rastLayer1, "[[:digit:]]+")
        gpsdata$harvestYear <-  str_extract(rastLayer2, "[[:digit:]]+")
      }
      
     return(gpsdata)
}





#################################################################################################################
#################################################################################################################


#' @description this functions loops through all .nc files (~30 -40 years) for SolarRadiation to provide point based data.
#' @details for AOI it requires a "AOI_GPS.RDS" data frame with c("longitude","latitude") columns being saved in 
#'                            paste("~/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/raw", sep="") 
#'          for trial sites it requires a "compiled_fieldData.RDS" data frame with c("lon", "lat", "plantingDate", "harvestDate") beinf saved in 
#'                    paste("~/agwise/AgWise_Data/fieldData_analytics/UseCase_",country, "_",useCaseName, "/", Crop, "/result", sep="")
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param overwrite default is FALSE 
#' @param Planting_month_date is needed only for AOI and should be provided as month_date, for trial locations the actual planting date is be used so no need to change the default value
#' @param Harvest_month_date is needed only for AOI and should be provided as month_date, for trial locations the actual harvest date is be used so no need to change the default value
#' @param jobs defines how many cores to use for parallel data sourcing
#' @param dataSource is among c("chirts", "AgEra")
#' @param ID only when AOI  = FALSE, it is the column name Identifying the trial ID in compiled_fieldData.RDS
#'  
#' @return a data frame containing the col information & columns corresponding to the SolarRadiation parameters#' 
#'        AvSolarRadiation : average SolarRadiation between pl_Date and hv_Date
#'        monthly_: monthly Solar Radiation for the growing season
#' @examples: get_SolarRadiation_pointSummarydata(country = "Rwanda";  useCaseName = "RAB"; Crop = "Potato"; AOI = FALSE; overwrite = TRUE;
#' Planting_month_date = "07-01";  Harvest_month_date = "11-30"; jobs=10)
get_SolarRadiation_pointSummarydata <- function(country, useCaseName, Crop, AOI = FALSE, overwrite = FALSE, 
                                         Planting_month_date = "02-01", Harvest_month_date = "05-30", 
                                         jobs = 10, dataSource, ID = NULL){
  
  ## define the directories store the result and also read list of .nc files 
  if(dataSource == "chirts"){
    listRaster_SR <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/SolarRadiation/chirts", pattern=".nc$", full.names = TRUE)
  }else if(dataSource == "AgEra"){
    listRaster_SR <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/SolarRadiation/AgEra", pattern=".nc$", full.names = TRUE)
  }
  
  pathOut1 <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/result/SolarRadiation", sep="")
  pathOut2 <- paste("/home/jovyan/agwise/AgWise_Data/potential_yield/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/SolarRadiation", sep="")
  pathOut3 <- paste("/home/jovyan/agwise/AgWise_Data/response_functions/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/SolarRadiation", sep="")
  
  
  
  if (!dir.exists(pathOut1)){
    dir.create(file.path(pathOut1), recursive = TRUE)
  }
  
  if (!dir.exists(pathOut2)){
    dir.create(file.path(pathOut2), recursive = TRUE)
  }
  
  if (!dir.exists(pathOut3)){
    dir.create(file.path(pathOut3), recursive = TRUE)
  }
  
  
  if(AOI == TRUE){
    countryCoord <- readRDS(paste("~/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/AOI_GPS.RDS", sep=""))
    countryCoord <- unique(countryCoord[, c("longitude", "latitude")])
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    
    ## check if both planting and harvest dates are in the same year
    Planting_month <- as.numeric(str_extract(Planting_month_date, "[^-]+"))
    harvest_month <- as.numeric(str_extract(Harvest_month_date, "[^-]+"))
    if(Planting_month < harvest_month){
      planting_harvest_sameYear <- TRUE
    }else{
      planting_harvest_sameYear <- FALSE
    }
    
    if (planting_harvest_sameYear ==TRUE){ #is used only to get the date of the year so the years 2001 and 2002 have no value except for formating 
      countryCoord$plantingDate <- paste(2001, Planting_month_date, sep="-")
      countryCoord$harvestDate <- paste(2001, Harvest_month_date, sep="-")
    }else{
      countryCoord$plantingDate <- paste(2001, Planting_month_date, sep="-")
      countryCoord$harvestDate <- paste(2002, Harvest_month_date, sep="-")
    }
    countryCoord <- countryCoord[complete.cases(countryCoord),]
    ground <- countryCoord
    
  }else{
    GPS_fieldData <- readRDS(paste("~/agwise/AgWise_Data/fieldData_analytics/UseCase_",country, "_",useCaseName, "/", Crop, "/result/compiled_fieldData.RDS", sep=""))  
    countryCoord <- unique(GPS_fieldData[, c("lon", "lat", "plantingDate", "harvestDate", ID)])
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    names(countryCoord) <- c("longitude", "latitude", "plantingDate", "harvestDate", ID)
    countryCoord <- countryCoord[complete.cases(countryCoord),]
    ground <- countryCoord
  }
  
  

  ground$Planting <- as.Date(ground$plantingDate, "%Y-%m-%d") # Planting date in Date format
  ground$Harvesting <- as.Date(ground$harvestDate, "%Y-%m-%d") # Harvesting date in Date format
  
  countryShp <- geodata::gadm(country, level = 3, path='.')
  dd2 <- raster::extract(countryShp, ground[, c("longitude", "latitude")])[, c("NAME_1", "NAME_2")]
  ground$NAME_1 <- dd2$NAME_1
  ground$NAME_2 <- dd2$NAME_2
  
  if(AOI == TRUE){
    
    ### 3.1.1 Convert planting Date and harvesting in Julian Day ####
    pl_j <-as.POSIXlt(unique(ground$Planting))$yday
    hv_j <-as.POSIXlt(unique(ground$Harvesting))$yday
    
    if (planting_harvest_sameYear ==  TRUE) {
      
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      
      ### 3.1.2 Read for the corresponding year and date
      SolarRadiation_result <- foreach(i=1:length(listRaster_SR), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        rast1 <- listRaster_SR[i]
        source("~/agwise/AgWise_Scripts/data_sourcing/get_geoSpatialSolarRadiation_MC.R", local = TRUE)
        summary_pointdata_SR(rastLayer1=rast1, rastLayer2=NULL, gpsdata = ground, pl_j=pl_j, hv_j=hv_j, planting_harvest_sameYear = TRUE)
      }
      SolarRadiation_points <- do.call(rbind, SolarRadiation_result)
      
      stopCluster(cls)
    }
    
    if (planting_harvest_sameYear ==  FALSE) {
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      
      ## SolarRadiation
      rf_result2 <- foreach(i = 1:(length(listRaster_SR)-1), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        listRaster_SR <- listRaster_SR[order(listRaster_SR)]
        source("~/agwise/AgWise_Scripts/data_sourcing/get_geoSpatialSolarRadiation_MC.R", local = TRUE)
        rast1 <- listRaster_SR[i]
        rast2 <- listRaster_SR[i+1]
        summary_pointdata_SR(rastLayer1=rast1, rastLayer2=rast2, gpsdata = ground, pl_j=pl_j, hv_j=hv_j, planting_harvest_sameYear = TRUE)
      }
      SolarRadiation_points <- do.call(rbind, rf_result2)
      
      stopCluster(cls)
      
    }
    
  }else {## when the planting and harvest dates varies for every row of data because it is actual trial data
    
    
    # 3 Loop on all the ID to calculate the parameters ####
    SolarRadiation_points <- NULL
    for(i in 1:nrow(ground)){
      print(i)
      # Extract the information for the i-th row
      groundi <- ground[i,]
      
      # Test if the cropping season overlaps two civil year
      yearPi <- format(as.POSIXlt(groundi$Planting), "%Y")
      yearHi <- format(as.POSIXlt(groundi$Harvesting), "%Y")
      ### 3.2.1 Convert planting Date and harvesting in Julian Day ####
      pl_j <-as.POSIXlt(groundi$Planting)$yday
      hv_j <-as.POSIXlt(groundi$Harvesting)$yday
      
      ## 3.1 Case same year ####
      if (yearPi == yearHi) {
        ### 3.1.2 Read for the corresponding year and date ####
        ## SolarRadiation
        SRi <-listRaster_SR[which(grepl(yearPi, listRaster_SR, fixed=TRUE) == T)]
        SRi <- terra::rast(SRi, lyrs=c(pl_j:hv_j))
      }
      
      ## 3.2 Case two years ####
      if (yearPi < yearHi) {
        
        ### 3.2.2 Read for the corresponding years and date ####
        ## SolarRadiation
        SRi1<-listRaster_SR[which(grepl(yearPi, listRaster_SR, fixed=TRUE) == T)]
        SRi1 <- terra::rast(SRi1, lyrs=c(pl_j:terra::nlyr(terra::rast(SRi1))))
        SRi2 <-listRaster_SR[which(grepl(yearHi, listRaster_SR, fixed=TRUE) == T)]
        SRi2 <- terra::rast(SRi2, lyrs=c(1:hv_j))
        SRi <- c(SRi1, SRi2)
       
      }
      
      ### 3.3 Extract the information for the i-th row ####
      xy <- groundi[, c("longitude", "latitude")]
      xy <- xy %>%
        mutate_if(is.character, as.numeric)
      
      SolarRadiation_points_i <- terra::extract(SRi, xy,method='simple', cells=FALSE)
      SolarRadiation_points_i <- SolarRadiation_points_i[, -1]
      
      ## get year
      groundi$Year <- yearPi
      
      # Compute the total amount of SolarRadiation
      groundi$AvSolarRadiation <- round(mean(as.numeric(SolarRadiation_points_i[c(1:length(SolarRadiation_points_i))])), digits=1)
      
    # Compute monthly SolarRadiation, at 31 days interval and the remaining  days at the end
     mdiv <- unique(c(seq(1, length(SolarRadiation_points_i), 30), length(SolarRadiation_points_i)))
      
      mrf <- c()
      for (k in 1:(length(mdiv)-1)) {
        if(k == 1){
          mrf <- c(mrf, mean(as.numeric(SolarRadiation_points_i[c(mdiv[k]:mdiv[k+1])])))
        }else{
          mrf <- c(mrf, mean(as.numeric(SolarRadiation_points_i[c((mdiv[k]+1):(mdiv[k+1]))])))
        }
      }
      
      if(length(mrf) > 6){## if the crop is > 6 month on the field
        mrf <- c(mrf, rep("NA", 6 -length(mrf)))
      }
      
      mrf_names <- c(paste0("solarRad_month", c(1:6)))
      for (h in 1:length(mrf_names)) {
        colname <- mrf_names[h]
        groundi[[colname]] <- mrf[h]
      }
      
      groundi <- subset(groundi, select=-c(Planting, Harvesting, Year))
      SolarRadiation_points <- rbind(SolarRadiation_points, groundi)
    }
  }
  
  SolarRadiation_points <- SolarRadiation_points %>% 
                                select_if(~sum(!is.na(.)) > 0)
  
  
  # 4 Writting of output: Check if the directory exists
  Planting_month_date <- gsub("-", "_", Planting_month_date)
  
  fname_SR <- ifelse(AOI == "TRUE", paste("SolarRadiation_summaries_AOI_" ,Planting_month_date, "_", dataSource, ".RDS",sep=""), paste("SolarRadiation_summaries_trial_", dataSource, ".RDS", sep=""))
 
  saveRDS(object = SolarRadiation_points, file=paste(pathOut1, fname_SR, sep="/"))
  saveRDS(object = SolarRadiation_points, file=paste(pathOut2, fname_SR, sep="/"))
  saveRDS(object = SolarRadiation_points, file=paste(pathOut3, fname_SR, sep="/"))  
  
  return(SolarRadiation_points)
  
}







#################################################################################################################
#################################################################################################################


#' @description this functions loops through all .nc files (~30 - 40 years) for Solar Radiation and provide point based data.
#' @details for AOI it requires a "AOI_GPS.RDS" data frame with c("longitude","latitude") columns being saved in 
#'                            paste("~/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/raw", sep="") 
#'          for trial sites it requires a "compiled_fieldData.RDS" data frame with c("lon", "lat", "plantingDate", "harvestDate") beinf saved in 
#'                    paste("~/agwise/AgWise_Data/fieldData_analytics/UseCase_",country, "_",useCaseName, "/", Crop, "/result", sep="")
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param overwrite default is FALSE 
#' @param Planting_month_date is needed only for AOI and should be provided as month_date, for trial locations the actual planting date is be used so no need to change the default value
#' @param Harvest_month_date is needed only for AOI and should be provided as month_date, for trial locations the actual harvest date is be used so no need to change the default value
#' @param jobs defines how many cores to use for parallel data sourcing
#' @param dataSource is one of c("chirts", "AgEra")
#' @param ID only when AOI  = FALSE, it is the column name Identifying the trial ID in compiled_fieldData.RDS
#' 
#' @return a data frame with file name made to reflect point/summary, AOI/trial, season, planting as mm_dd and source chirts/AgEra
#' the data frame contains daily Solar Radiation for every GPS point. 
#' For the trial sites: it provides:longitude, latitude, plantingDate and harvestDate as yyyy-mm-dd, yearPi & yearHi as yyyy, 
#' pl_j & hv_j as date of the year for panting & harvest, growinglength and daily Solar Radiation by date, NA for dates not part of the growing season for a trial
#' For AOI: it provides daily Solar Radiation for the dates between Planting_month_date and Harvest_month_dates plus 
#' plantingYear, harvestYear, longitude, latitude   
#' 
#' @examples: get_SolarRadiation_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE, overwrite = TRUE,
#'             Planting_month_date = "07-01",  Harvest_month_date = "11-30", 
#'             jobs=10,  dataSource = "AgEra")
get_SolarRadiation_pointData <- function(country, useCaseName, Crop, AOI = FALSE, overwrite = FALSE, 
                               Planting_month_date = "02-01", Harvest_month_date = "05-30", 
                               jobs = 10, dataSource, ID){
  
  if(dataSource == "chirts"){
    listRaster_SR <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/SolarRadiation/chirts", pattern=".nc$", full.names = TRUE)
  }else if(dataSource == "AgEra"){
    listRaster_SR <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/SolarRadiation/AgEra", pattern=".nc$", full.names = TRUE)
  }
  
  pathOut1 <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/result/SolarRadiation", sep="")
  pathOut2 <- paste("/home/jovyan/agwise/AgWise_Data/potential_yield/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/SolarRadiation", sep="")
  pathOut3 <- paste("/home/jovyan/agwise/AgWise_Data/response_functions/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/SolarRadiation", sep="")
  
  
  if (!dir.exists(pathOut1)){
    dir.create(file.path(pathOut1), recursive = TRUE)
  }
  
  if (!dir.exists(pathOut2)){
    dir.create(file.path(pathOut2), recursive = TRUE)
  }
  
  if (!dir.exists(pathOut3)){
    dir.create(file.path(pathOut3), recursive = TRUE)
  }
  
  
  
  if(AOI == TRUE){
    countryCoord <- readRDS(paste("~/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/AOI_GPS.RDS", sep=""))
    countryCoord <- unique(countryCoord[, c("longitude", "latitude")])
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    
    ## check if both planting and harvest dates are in the same year
    Planting_month <- as.numeric(str_extract(Planting_month_date, "[^-]+"))
    harvest_month <- as.numeric(str_extract(Harvest_month_date, "[^-]+"))
    if(Planting_month < harvest_month){
      planting_harvest_sameYear <- TRUE
    }else{
      planting_harvest_sameYear <- FALSE
    }
    
    if (planting_harvest_sameYear ==TRUE){ #is used only to get the date of the year so the years 2001 and 2002 have no value except for formating 
      countryCoord$plantingDate <- paste(2001, Planting_month_date, sep="-")
      countryCoord$harvestDate <- paste(2001, Harvest_month_date, sep="-")
    }else{
      countryCoord$plantingDate <- paste(2001, Planting_month_date, sep="-")
      countryCoord$harvestDate <- paste(2002, Harvest_month_date, sep="-")
    }
    countryCoord <- countryCoord[complete.cases(countryCoord),]
    ground <- countryCoord[, c("longitude", "latitude", "plantingDate", "harvestDate")]
  }else{
    GPS_fieldData <- readRDS(paste("~/agwise/AgWise_Data/fieldData_analytics/UseCase_",country, "_",useCaseName, "/", Crop, "/result/compiled_fieldData.RDS", sep=""))  
    countryCoord <- unique(GPS_fieldData[, c("lon", "lat", "plantingDate", "harvestDate", ID)])
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    names(countryCoord) <- c("longitude", "latitude", "plantingDate", "harvestDate", ID)
    ground <- countryCoord
  }
  
  
  
  # 2. the ground data ####
  ground$Planting <- as.Date(ground$plantingDate, "%Y-%m-%d") # Planting date in Date format
  ground$Harvesting <- as.Date(ground$harvestDate, "%Y-%m-%d") # Harvesting date in Date format
  
  ## add the top two levels of adminstration unit 
  countryShp <- geodata::gadm(country, level = 3, path='.')
  dd2 <- raster::extract(countryShp, ground[, c("longitude", "latitude")])[, c("NAME_1", "NAME_2")]
  ground$NAME_1 <- dd2$NAME_1
  ground$NAME_2 <- dd2$NAME_2
  
  
  if(AOI == TRUE){
    
    ### 3.1.1 Convert planting Date and harvesting in Julian Day ####
    pl_j <-as.POSIXlt(unique(ground$Planting))$yday
    hv_j <-as.POSIXlt(unique(ground$Harvesting))$yday
    
    if (planting_harvest_sameYear ==  TRUE) {
      
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      
      ### 3.1.2 Read for the corresponding year and date. Avoid the last year because it is not complete for 365 days
      SR_result <- foreach(i=1:(length(listRaster_SR)-1), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        rasti <- listRaster_SR[i]
        PlHvD <- terra::rast(rasti, lyrs=c(pl_j:hv_j))
        xy <- ground[, c("longitude", "latitude")]
        SRi <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
        SRi <- SRi[,-1]
        names(SRi) <- paste("SR", sub("^[^_]+", "", names(SRi)), sep="")
        SRi$plantingYear <-  str_extract(rasti, "[[:digit:]]+")
        SRi$harvestYear <- str_extract(rasti, "[[:digit:]]+")
        ground2 <- cbind(ground, SRi)
      }
      SolarRadiation_points <- do.call(rbind, SR_result)
      
      stopCluster(cls)
    }
    
    
    if (planting_harvest_sameYear ==  FALSE) {
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      ## SolarRadiation
      SR_result2 <- foreach(i = 1:(length(listRaster_SR)-1), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        listRaster_SR <- listRaster_SR[order(listRaster_SR)]
        rast1 <- listRaster_SR[i]
        rast2 <- listRaster_SR[i+1]
        rasti1 <- terra::rast(rast1, lyrs=c(pl_j:terra::nlyr(terra::rast(rast1))))
        rasti2 <- terra::rast(rast2, lyrs=c(1:hv_j))
        PlHvD <- c(rasti1, rasti2)
        xy <- ground[, c("longitude", "latitude")]
        SRi <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
        SRi <- SRi[,-1]
        
        names(SRi) <- sub("^[^_]+", "", names(SRi))
        if(length(grep("_366", names(SRi))) > 0){
          SRi <- SRi[,-grep("_366", names(SRi))]
        }
        names(SRi) <- paste("SR", names(SRi), sep="")
        SRi$plantingYear <- str_extract(rast1, "[[:digit:]]+")
        SRi$harvestYear <- str_extract(rast2, "[[:digit:]]+")
        ground2 <- cbind(ground, SRi)
      }
      
      SolarRadiation_points <- do.call(rbind, SR_result2)
      
      stopCluster(cls)
      
    }
    
  }else {## when the planting and harvest dates varies for every row of data because it is actual trial data
    
    ground$yearPi <- format(as.POSIXlt(ground$Planting), "%Y")
    ground$yearHi <- format(as.POSIXlt(ground$Harvesting), "%Y")
    ### 3.2.1 Convert planting Date and harvesting in Julian Day ####
    ground$ pl_j <-as.POSIXlt(ground$Planting)$yday
    ground$hv_j <-as.POSIXlt(ground$Harvesting)$yday
    
    ## get the max nr of days on the field to use is to create column names. Given trials can have different start and end dates, the column names does not match if we use the date of the year
    ground$growinglength <- ifelse(ground$yearPi == ground$yearHi, 
                                   ground$hv_j - ground$pl_j,
                                   365 - ground$pl_j + ground$hv_j)
    
    ## create list of all possible column names to be able to rbind data from different sites with different planting and harvest dates
    SR_names <- c(paste0("SR_", c(min(ground$pl_j):max(ground$hv_j))))
    SR_names2 <-  as.data.frame(matrix(nrow=length(SR_names), ncol=1))
    colnames(SR_names2) <- "ID"
    SR_names2$ID <- c(1:nrow(SR_names2))
    SR_names2[,2] <- SR_names
    
    
    # 3 Loop on all the ID to calculate the parameters ####
    SolarRadiation_points <- NULL
    for(i in 1:nrow(ground)){
      print(i)
      # Extract the information for the i-th row
      # groundi <- ground[i, c("longitude", "latitude", ID, "plantingDate", "harvestDate","yearPi", "yearHi", "pl_j", "hv_j", "growinglength")]
      groundi <- ground[i, c("longitude", "latitude", "plantingDate", "harvestDate",ID,"NAME_1", "NAME_2","yearPi", "yearHi", "pl_j", "hv_j", "growinglength")]
      
      
       # Test if the cropping season overlaps two civil year
      yearPi <- groundi$yearPi
      yearHi <- groundi$yearHi
      ### 3.2.1 Convert planting Date and harvesting in Julian Day ####
      pl_j <- groundi$pl_j
      hv_j <- groundi$hv_j
      
      
      ## 3.1 Case same year ####
      if (yearPi == yearHi) {
        rasti<-listRaster_SR[which(grepl(yearPi, listRaster_SR, fixed=TRUE) == T)]
        rasti <- terra::rast(rasti, lyrs=c(pl_j:hv_j))
      }
      
      ## 3.2 Case two years ####
      if (yearPi < yearHi) {
        rasti1<-listRaster_SR[which(grepl(yearPi, listRaster_SR, fixed=TRUE) == T)]
        rasti1 <- terra::rast(rasti1, lyrs=c(pl_j:terra::nlyr(terra::rast(rasti1))))
        rasti2 <-listRaster_SR[which(grepl(yearHi, listRaster_SR, fixed=TRUE) == T)]
        rasti2 <- terra::rast(rasti2, lyrs=c(1:hv_j))
        rasti <- c(rasti1, rasti2)
        
      }
      
      ### 3.3 Extract the information for the i-th row ####
      xy <- groundi[, c("longitude", "latitude")]
      xy <- xy %>%
        mutate_if(is.character, as.numeric)
      
      SRi <- terra::extract(rasti, xy,method='simple', cells=FALSE)
      SRi <- SRi[,-1]
      names(SRi) <- sub("^[^_]+", "", names(SRi))
      names(SRi) <- paste("SR", names(SRi), sep="")
      SRi <- as.data.frame(t(SRi))
      SRi$V2 <- rownames(SRi)
      rownames(SRi) <- NULL
      
      SRi <- merge(SRi,SR_names2, by="V2", all.y = TRUE)
      SRi <- SRi[order(SRi$ID),]
      SRi <- SRi[,-3]
      SRi2 <- as.data.frame(t(SRi))
      colnames(SRi2) <- SRi$V2
      SRi2 <- SRi2[-1,]
      SRi2 <- cbind(groundi, SRi2)
      SolarRadiation_points <- rbind(SolarRadiation_points, SRi2)
    }
  }
  
  
  # 4 Writting of output: Check if the directory exists
  Planting_month_date <- gsub("-", "_", Planting_month_date)
  
  
  fname_SR <- ifelse(AOI == "TRUE", paste("SolarRadiation_pointData_AOI_" ,Planting_month_date, "_", dataSource, ".RDS",sep=""), paste("SolarRadiation_pointData_trial_", dataSource, ".RDS", sep=""))
  
  saveRDS(object = SolarRadiation_points, file=paste(pathOut1, fname_SR, sep="/"))
  saveRDS(object = SolarRadiation_points, file=paste(pathOut2, fname_SR, sep="/"))
  saveRDS(object = SolarRadiation_points, file=paste(pathOut3, fname_SR, sep="/")) 
  return(SolarRadiation_points)
  
}



