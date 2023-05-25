
# 1. Sourcing required packages -------------------------------------------
#################################################################################################################
packages_required <- c("doParallel", "foreach", "chirps", "tidyverse", "dplyr", "lubridate", "stringr", "terra", "countrycode", "sf")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))



#################################################################################################################
#################################################################################################################
# 2. Function to crop the rainfall data over the use case extent -------------------------------------------
#' @description a function to crop the rainfall global layer but this does duplicate a large volume of data , it is better directly to source from the global data
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param dataSource is one of c("CHIRPS", "AgEra")
#' @param overwrite default is FALSE 
#'
#' @return raster files cropped from global data and the result will be written out in useCaseName/Crop/raw/Rainfall/
#'
#' @examples get_geoSpatial_rainfall(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", dataSource = "CHIRPS",overwrite = TRUE)
crop_geoSpatial_rainfall <- function(country, useCaseName, Crop, dataSource, overwrite){
  
  # 2.1. Prepare the output & read the rainfall data #### 
  #TODO create a look up table to check use case - country names
  ## get country abbreviation to used in gdam function
  countryCC <- countrycode(country, origin = 'country.name', destination = 'iso3c')
  
  ## create a directory to store the cropped data: 
  if(dataSource == "CHIRPS"){
    pathOut <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/Rainfall/CHIRPS", sep="")
  
    ## read rainfall layers and crop
    listRaster_rf <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/Rainfall/chirps", pattern=".nc$")
    readLayers_rf <- terra::rast(paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/Rainfall/chirps", listRaster_rf, sep="/"))
    fileName <- "/CHIRPS_geospatial_Rainfall.tif"
    
  }else{
    pathOut <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/Rainfall/AgEra", sep="")
    listRaster_rf <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/Rainfall/AgEra", pattern=".nc$")
    readLayers_rf <- terra::rast(paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/Rainfall/AgEra", listRaster_rf, sep="/"))
    fileName <- "/AgEra_geospatial_Rainfall.tif"
  }
  
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }
  
  ## read the relevant shape file from gdam to be used to crop the global data
  countryShp <- geodata::gadm(countryCC, level = 3, path='.')
  
  # 2.2. Cropped the rainfall layers and save the results #### 
  ## crop the layers 
  croppedLayer_rf <- terra::crop(readLayers_rf, countryShp)

  ## save result
  terra::writeRaster(croppedLayer_rf, paste(pathOut, fileName , sep="/"), filetype="GTiff", overwrite = overwrite)
  
  return(croppedLayer_rf)
}



#################################################################################################################
#################################################################################################################

# 3. Function to get seasonal rainfall parameters for point data over the cropping season  -------------------------------------------
#' @description is a function to get total rainfall, number of rainy days and monthly rainfall, and working when the planting and harvest happen in different years
#' @param raster1 the .nc file for the planting year, within get_rf_pointdata function, this is provided by the function 
#' @param raster2 the .nc file for the harvest year, within get_rf_pointdata function, this is provided by the function 
#' @param gpsdata a data frame with longitude and latitude 
#' @param pl_j the planting date as the date of the year
#' @param hv_j the harvest date as the date of the year
#'
#' @return  a data frame with total rainfall, number of rainy days and monthly rainfall
#' @example summary_pointdata_rainfall(rastLayer1="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/Rainfall/chirps/1981.nc",
                   # raster2="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/Rainfall/chirps/1982.nc",
                   # gpsdata=data.frame(longitude = c(29.375, 30.125), latitude = c(-2.825, -2.425)),  
                   # pl_j=35, hv_j=150, planting_harvest_sameYear = TRUE)

summary_pointdata_rainfall <- function(rastLayer1=NULL, rastLayer2=NULL, gpsdata, pl_j, hv_j, planting_harvest_sameYear){
  
  # 3.1. Read the rainfall data and shape the ground data ####
  if(planting_harvest_sameYear == TRUE){
    PlHvD <- terra::rast(rastLayer1, lyrs=c(pl_j:hv_j)) 
  }else{
    rasti1 <- terra::rast(rastLayer1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastLayer1))))
    rasti2 <- terra::rast(rastLayer2, lyrs=c(1:hv_j))
    PlHvD <- c(rasti1, rasti2)
  }
 
  xy <- gpsdata[, c("longitude", "latitude")]
  raini <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
  raini <- raini[,-1]
  
  # 3.2. Get the rainfall seasonal parameters at a location ####
  ## The total rainfall over the growing period
  
  rainiq <- t(raini[c(2:length(raini))])
  gpsdata$totalRF <- colSums(rainiq)
    
  ## The number of rainy days (thr= 2 mm) over the growing period 
  gpsdata$nrRainyDays <- NULL
    for (m in 1:nrow(raini)){
      print(m)
      mdata <- raini[m, ]
      mdata[mdata < 2] <- 0
      mdata[mdata >= 2] <- 1
      gpsdata$nrRainyDays[m] <- sum(mdata)
      
  ## The monthly rainfall, at 31 days interval and the remaining  days at the end, over the growing period
      mrdi <- raini[m, ]
      mdiv <- unique(c(seq(1, length(mrdi), 30), length(mrdi)))
      
      mrf <- c()
      for (k in 1:(length(mdiv)-1)) {
        print(k)
        if(k == 1){
          mrf <- c(mrf, sum(mrdi[c(mdiv[k]:mdiv[k+1])]))
        }else{
          mrf <- c(mrf, sum(mrdi[c((mdiv[k]+1):(mdiv[k+1]))]))
        }
      }
      
      if(length(mrf) > 6){## if the crop is > 6 month on the field
        mrf <- c(mrf, rep("NA", 6 -length(mrf)))
      }
      
      mrf_names <- c(paste0("Rain_month", c(1:6)))
      for (h in 1:length(mrf_names)) {
        colname <- mrf_names[h]
        gpsdata[[colname]][m] <- mrf[h]
      }
    }
  
  if(planting_harvest_sameYear== TRUE){
    gpsdata$plantingYear <- str_extract(rastLayer1, "[[:digit:]]+")
    gpsdata$harvestYear <- str_extract(rastLayer1, "[[:digit:]]+")
  }else{
    gpsdata$plantingYear <- str_extract(rastLayer1, "[[:digit:]]+")
    gpsdata$harvestYear <- str_extract(rastLayer2, "[[:digit:]]+")
  }
 


  return(gpsdata)
}



#################################################################################################################
#################################################################################################################
# 4. Extract rainfall data time series for point based data -------------------------------------------
#' @description this functions loops through all .nc files (~30 - 40 years) for rainfall to provide point based data.
#' @details for AOI it requires a "AOI_GPS.RDS" data frame with c("longitude","latitude") columns being saved in 
#'                            paste("~/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/raw", sep="") 
#'          for trial sites it requires a "compiled_fieldData.RDS" data frame with c("lon", "lat", "plantingDate", "harvestDate") beinf saved in 
#'                    paste("~/agwise/AgWise_Data/fieldData_analytics/UseCase_",country, "_",useCaseName, "/", Crop, "/result", sep="")
#'  
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param overwrite default is FALSE 
#' @param Planting_month_date is needed only for AOI and should be provided as month_date, for trial locations the actual planting date is be used so no need to change the default value
#' @param Harvest_month_date is needed only for AOI and should be provided as month_date, for trial locations the actual harvest date is be used so no need to change the default value
#' @param jobs defines how many cores to use for parallel data sourcing
#' @param season used to formulate a file name and is useful when data for different seasons is needed 
#' @param dataSource is one of c("CHIRPS", "AgEra")
#' @param ID only when AOI  = FALSE, it is the column name Identifying the trial ID in compiled_fieldData.RDS
#' 
#' @return a data frame with file name made to reflect point/summary, AOI/trial, season, planting as mm_dd and source CHIRPS/AgEra
#' the data frame contains daily rainfall for every GPS point. 
#' For the trial sites: it provides:longitude, latitude, plantingDate and harvestDate as yyyy-mm-dd, yearPi & yearHi as yyyy, 
#' pl_j & hv_j as date of the year for panting & harvest, growinglength and daily rainfall by date, NA for dates not part of the growing season for a trial
#' For AOI: it provides dily rainfall for ll dates between Planting_month_date and Harvest_month_dates plus 
#' plantingYear, harvestYear, longitude, latitude   
#' 
#' @examples: get_summaries(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE, overwrite = TRUE,
#'             season="season1", Planting_month_date = "07-01",  Harvest_month_date = "11-30", jobs=10, id = "TLID")
get_rf_pointData <- function(country, useCaseName, Crop, AOI = FALSE, overwrite = FALSE, 
                                  Planting_month_date = "02-01", Harvest_month_date = "05-30", 
                                  jobs = 10, season="season_1", dataSource, ID){
 
  # 4.1. Initialization of input and output data ####
  
   # Input rainfall
  if(dataSource == "CHIRPS"){
    listRaster_RF <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/Rainfall/chirps", pattern=".nc$", full.names = TRUE)
  }else{
    listRaster_RF <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/Rainfall/AgEra", pattern=".nc$", full.names = TRUE)
  }
  
  # Creation of the output dir
  pathOut1 <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/result/Rainfall", sep="")
  pathOut2 <- paste("/home/jovyan/agwise/AgWise_Data/potential_yield/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/Rainfall", sep="")
  pathOut3 <- paste("/home/jovyan/agwise/AgWise_Data/response_functions/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/Rainfall", sep="")

  if (!dir.exists(pathOut1)){
    dir.create(file.path(pathOut1), recursive = TRUE)
  }
  
   if (!dir.exists(pathOut2)){
    dir.create(file.path(pathOut2), recursive = TRUE)
   }
  
  if (!dir.exists(pathOut3)){
    dir.create(file.path(pathOut3), recursive = TRUE)
  }
  
  # Input point data AOI / Trial
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
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    ground <- countryCoord[, c("longitude", "latitude", "plantingDate", "harvestDate")]
    
  }else{
    GPS_fieldData <- readRDS(paste("~/agwise/AgWise_Data/fieldData_analytics/UseCase_",country, "_",useCaseName, "/", Crop, "/result/compiled_fieldData.RDS", sep=""))  
    countryCoord <- unique(GPS_fieldData[, c("lon", "lat", "plantingDate", "harvestDate", ID)])
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    names(countryCoord) <- c("longitude", "latitude", "plantingDate", "harvestDate", ID)
    ground <- countryCoord
  }
 


  ground$Planting <- as.Date(ground$plantingDate, "%Y-%m-%d") # Planting date in Date format
  ground$Harvesting <- as.Date(ground$harvestDate, "%Y-%m-%d") # Harvesting date in Date format
  
  countryShp <- geodata::gadm(country, level = 3, path='.')
  dd2 <- raster::extract(countryShp, ground[, c("longitude", "latitude")])[, c("NAME_1", "NAME_2")]
  ground$NAME_1 <- dd2$NAME_1
  ground$NAME_2 <- dd2$NAME_2

  # 4.2. Compute the seasonal rainfall parameters for AOI ####
  
  if(AOI == TRUE){
    
    # Convert planting Date and harvesting in Julian Day 
    pl_j <-as.POSIXlt(unique(ground$Planting))$yday
    hv_j <-as.POSIXlt(unique(ground$Harvesting))$yday
    
    ## 4.2.1. Case planting and harvesting dates span the same year ####
    if (planting_harvest_sameYear ==  TRUE) {
      
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      
      # Loop on all the year 
      rf_result <- foreach(i=1:length(listRaster_RF), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        rasti <- listRaster_RF[i]
        PlHvD <- terra::rast(rasti, lyrs=c(pl_j:hv_j))
        xy <- ground[, c("longitude", "latitude")]
        raini <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
        raini <- raini[,-1]
        names(raini) <- paste("Rain", sub("^[^_]+", "", names(raini)), sep="")
        raini$plantingYear <- raini$harvestYear <- str_extract(rasti, "[[:digit:]]+")
        ground2 <- cbind(ground, raini)
      }
      
      rainfall_points <- do.call(rbind, rf_result)
      stopCluster(cls)
    }
    
    ## 4.2.2. Case planting and harvesting dates span two different years ####
    if (planting_harvest_sameYear ==  FALSE) {
      
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
     ## Rainfall
        rf_result2 <- foreach(i = 1:(length(listRaster_RF)-1), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        listRaster_RF <- listRaster_RF[order(listRaster_RF)]
        rast1 <- listRaster_RF[i]
        rast2 <- listRaster_RF[i+1]
        rasti1 <- terra::rast(rast1, lyrs=c(pl_j:terra::nlyr(terra::rast(rast1))))
        rasti2 <- terra::rast(rast2, lyrs=c(1:hv_j))
        PlHvD <- c(rasti1, rasti2)
        xy <- ground[, c("longitude", "latitude")]
        raini <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
        raini <- raini[,-1]
        
        names(raini) <- sub("^[^_]+", "", names(raini))
        if(length(grep("_366", names(raini))) > 0){
          raini <- raini[,-grep("_366", names(raini))]
        }
        names(raini) <- paste("Rain", names(raini), sep="")
        raini$plantingYear <- str_extract(rast1, "[[:digit:]]+")
        raini$harvestYear <- str_extract(rast2, "[[:digit:]]+")
        ground2 <- cbind(ground, raini)
     
      }
      
      rainfall_points <- do.call(rbind, rf_result2)
      stopCluster(cls)
      
    }
  
    # 4.3. Compute the seasonal rainfall parameters for trial data ####
    # when the planting and harvest dates varies for every row of data because it is actual trial data
  }else {
    
    ## 4.3.1. Get the planting and harvesting dates ####
    # Get the Year
    ground$yearPi <- format(as.POSIXlt(ground$Planting), "%Y")
    ground$yearHi <- format(as.POSIXlt(ground$Harvesting), "%Y")
    
    # Convert planting date and harvesting date in Julian Day
    ground$ pl_j <-as.POSIXlt(ground$Planting)$yday
    ground$hv_j <-as.POSIXlt(ground$Harvesting)$yday
    
    # get the max number of days on the field to use is to create column names. Given trials can have different start and end dates, the column names does not match if we use the date of the year
    ground$growinglength <- ifelse(ground$yearPi == ground$yearHi, 
                                      ground$hv_j - ground$pl_j,
                                      365 - ground$pl_j + ground$hv_j)
    
    # create list of all possible column names to be able to rbind data from different sites with different planting and harvest dates
    rf_names <- c(paste0("Rain_", c(min(ground$pl_j):max(ground$hv_j))))
    rf_names2 <-  as.data.frame(matrix(nrow=length(rf_names), ncol=1))
    colnames(rf_names2) <- "ID"
    rf_names2$ID <- c(1:nrow(rf_names2))
    rf_names2[,2] <- rf_names
    
    ## 4.3.2. Loop on all the trial location to calculate the seasonal rainfall parameters ####

    rainfall_points <- NULL
    for(i in 1:nrow(ground)){
      print(i)
      
      # Extract the i-th row
      groundi <- ground[i, c("longitude", "latitude", "plantingDate", "harvestDate",ID,"NAME_1", "NAME_2","yearPi", "yearHi", "pl_j", "hv_j", "growinglength")]
      
      # Extract the Year 
      yearPi <- groundi$yearPi
      yearHi <- groundi$yearHi
      
      # Convert planting Date and harvesting in Julian Day
      pl_j <- groundi$pl_j
      hv_j <- groundi$hv_j
      
     
      ### 4.3.2.1. Subset the rainfall data according to the length of the growing season ####
      # Case planting and harvesting dates span the same year
      
      if (yearPi == yearHi) {
        rasti<-listRaster_RF[which(grepl(yearPi, listRaster_RF, fixed=TRUE) == T)]
        rasti <- terra::rast(rasti, lyrs=c(pl_j:hv_j))
      }
      
      # Case planting and harvesting dates span two different years
      if (yearPi < yearHi) {
        rasti1<-listRaster_RF[which(grepl(yearPi, listRaster_RF, fixed=TRUE) == T)]
        rasti1 <- terra::rast(rasti1, lyrs=c(pl_j:terra::nlyr(terra::rast(rasti1))))
        rasti2 <-listRaster_RF[which(grepl(yearHi, listRaster_RF, fixed=TRUE) == T)]
        rasti2 <- terra::rast(rasti2, lyrs=c(1:hv_j))
        rasti <- c(rasti1, rasti2)
      
      }
      
      ### 4.3.2.2.Extract the information for the i-th row ####
      
      xy <- groundi[, c("longitude", "latitude")]
      xy <- xy %>%
       mutate_if(is.character, as.numeric)
       
      raini <- terra::extract(rasti, xy,method='simple', cells=FALSE)
      raini <- raini[,-1]
      names(raini) <- sub("^[^_]+", "", names(raini))
      names(raini) <- paste("Rain", names(raini), sep="")
      raini <- as.data.frame(t(raini))
      raini$V2 <- rownames(raini)
      rownames(raini) <- NULL
      
      raini <- merge(raini, rf_names2, by="V2", all.y = TRUE)
      raini <- raini[order(raini$ID),]
      raini <- raini[,-3]
      raini2 <- as.data.frame(t(raini))
      colnames(raini2) <- raini$V2
      raini2 <- raini2[-1,]
      
      raini2 <- cbind(groundi, raini2)
      rainfall_points <- rbind(rainfall_points, raini2)
    }
  }
  
 # 4.4. Writting the output ####
  
  # Check if the directory exists
  Planting_month_date <- gsub("-", "_", Planting_month_date)
   fname_rain <- ifelse(AOI == "TRUE", paste("Rainfall_pointData_AOI_", season,"_" ,Planting_month_date, "_", dataSource, ".RDS",sep=""), paste("Rainfall_pointData_trial_", dataSource, ".RDS", sep=""))
  
  saveRDS(object = rainfall_points, file=paste(pathOut1, fname_rain, sep="/"))
  saveRDS(object = rainfall_points, file=paste(pathOut2, fname_rain, sep="/"))
  saveRDS(object = rainfall_points, file=paste(pathOut3, fname_rain, sep="/")) 
  return(rainfall_points)
}

#################################################################################################################
#################################################################################################################
# 5. Extract the season rainfall parameters for point based data -------------------------------------------
#' @description this functions loops through all .nc files (~30 -40 years) for rainfall to provide point based seasonal rainfall parameters.
#' @details for AOI it requires a "AOI_GPS.RDS" data frame with c("longitude","latitude") columns being saved in 
#'                            paste("~/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/raw", sep="") 
#'          for trial sites it requires a "compiled_fieldData.RDS" data frame with c("lon", "lat", "plantingDate", "harvestDate") beinf saved in 
#'                    paste("~/agwise/AgWise_Data/fieldData_analytics/UseCase_",country, "_",useCaseName, "/", Crop, "/result", sep="")
#'
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param overwrite default is FALSE 
#' @param Planting_month_date is needed only for AOI and should be Rain_monthly provided as month_date, for trial locations the actual planting date is be used so no need to change the default value
#' @param Harvest_month_date is needed only for AOI and should be provided as month_date, for trial locations the actual harvest date is be used so no need to change the default value
#' @param jobs defines how many cores to use for parallel data sourcing
#' @param season used to formulate a file name and is useful when data for different seasons is needed 
#' @param dataSource is among c("CHIRPS", "AgEra")
#' @param ID only when AOI  = FALSE, it is the column name Identifying the trial ID in compiled_fieldData.RDS
#' 
#' @return a data frame containing the col information & columns corresponding to the rainfall parameters#' 
#'        totalRF : Total rainfall between pl_Date and hv_Date (mm)
#'        nrRainyDays : Number of rainy days between pl_Date and hv_Date (days)
#'        di : Average daily rainfall between pl_Date and hv_Date (mm/day)
#'        Rain_monthx: total monthly rainfall 
#' @examples: get_rf_pointSummarydata(country = "Rwanda";  useCaseName = "RAB"; Crop = "Potato"; AOI = FALSE; overwrite = TRUE;
#' season="season1";Planting_month_date = "07-01";  Harvest_month_date = "11-30";jobs=10, id = "TLID")
get_rf_pointSummarydata <- function(country, useCaseName, Crop, AOI = FALSE, overwrite = FALSE, 
                                         Planting_month_date = "02-01", Harvest_month_date = "05-30", 
                                         jobs = 10, season="season_1", dataSource, ID){
  
 
  # 5.1. Initialization of input and output data ####
  
  # Input rainfall
  if(dataSource == "CHIRPS"){
    listRaster_RF <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/Rainfall/chirps", pattern=".nc$", full.names = TRUE)
  }else{
    listRaster_RF <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/Rainfall/AgEra", pattern=".nc$", full.names = TRUE)
  }
  
  # Creation of the output dir
  pathOut1 <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/result/Rainfall", sep="")
  pathOut2 <- paste("/home/jovyan/agwise/AgWise_Data/potential_yield/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/Rainfall", sep="")
  pathOut3 <- paste("/home/jovyan/agwise/AgWise_Data/response_functions/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/Rainfall", sep="")
  
  if (!dir.exists(pathOut1)){
    dir.create(file.path(pathOut1), recursive = TRUE)
  }
  
  if (!dir.exists(pathOut2)){
    dir.create(file.path(pathOut2), recursive = TRUE)
  }
  
  if (!dir.exists(pathOut3)){
    dir.create(file.path(pathOut3), recursive = TRUE)
  }
  
  # Input point data AOI / Trial
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
  
  
  ground$Planting <- as.Date(ground$plantingDate, "%Y-%m-%d") # Planting date in Date format
  ground$Harvesting <- as.Date(ground$harvestDate, "%Y-%m-%d") # Harvesting date in Date format
  
  countryShp <- geodata::gadm(country, level = 3, path='.')
  dd2 <- raster::extract(countryShp, ground[, c("longitude", "latitude")])[, c("NAME_1", "NAME_2")]
  ground$NAME_1 <- dd2$NAME_1
  ground$NAME_2 <- dd2$NAME_2
  
  # 5.2. Compute the seasonal rainfall parameters for AOI ####
  if(AOI == TRUE){
    
    # Convert planting Date and harvesting in Julian Day 
    pl_j <-as.POSIXlt(unique(ground$Planting))$yday
    hv_j <-as.POSIXlt(unique(ground$Harvesting))$yday
    
    ## 5.2.1. Case planting and harvesting dates span the same year ####
    if (planting_harvest_sameYear ==  TRUE) {
      
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      
      # Loop over all the years 
      rf_result <- foreach(i=1:(length(listRaster_RF)-1), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        rast1 <- listRaster_RF[i]
        source("~/agwise/AgWise_Scripts/data_sourcing/get_geoSpatialRainfall.R", local = TRUE)
        summary_pointdata_rainfall(rastLayer1=rast1, rastLayer2 = NULL, gpsdata = ground, pl_j=pl_j, hv_j=hv_j, 
                                   planting_harvest_sameYear = TRUE)
      }
      rainfall_points <- do.call(rbind, rf_result)
      
    }
  
    ## 5.2.2. Case planting and harvesting dates span two different years ####
    if (planting_harvest_sameYear ==  FALSE) {
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      
      ## Rainfall
      rf_result2 <- foreach(i = 1:(length(listRaster_RF)-1), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        listRaster_RF <- listRaster_RF[order(listRaster_RF)]
        source("~/agwise/AgWise_Scripts/data_sourcing/get_geoSpatialRainfall.R", local = TRUE)
        rast1 <- listRaster_RF[i]
        rast2 <- listRaster_RF[i+1]
        summary_pointdata_rainfall(rastLayer1=rast1, rastLayer2 = rast2, gpsdata = ground, pl_j=pl_j, hv_j=hv_j, 
                                   planting_harvest_sameYear = TRUE)
      }
      rainfall_points <- do.call(rbind, rf_result2)
      
    }
    
    stopCluster(cls)
    # 5.3. Compute the seasonal rainfall parameters for trial data ####
    # when the planting and harvest dates varies for every row of data because it is actual trial data
  }else {
    
    
    # Loop on all the ID to calculate the parameters 
    rainfall_points <- NULL
    for(i in 1:nrow(ground)){
      print(i)
      
      # Extract the information for the i-th row
      groundi <- ground[i,]
      
      # Get the years
      yearPi <- format(as.POSIXlt(groundi$Planting), "%Y")
      yearHi <- format(as.POSIXlt(groundi$Harvesting), "%Y")
      
      # Convert planting Date and harvesting in Julian Day 
      pl_j <-as.POSIXlt(groundi$Planting)$yday
      hv_j <-as.POSIXlt(groundi$Harvesting)$yday
      
      ## 5.3.1. Case planting and harvesting dates span the same year ####
      if (yearPi == yearHi) {
        
        # Read for the corresponding year and date 
        rasti<-listRaster_RF[which(grepl(yearPi, listRaster_RF, fixed=TRUE) == T)]
        rasti <- terra::rast(rasti, lyrs=c(pl_j:hv_j))
      }
      
      ## 5.3.2 Case planting and harvesting dates span two different years ####
      if (yearPi < yearHi) {
        
        # Read for the corresponding years and date 
        rasti1<-listRaster_RF[which(grepl(yearPi, listRaster_RF, fixed=TRUE) == T)]
        rasti1 <- terra::rast(rasti1, lyrs=c(pl_j:terra::nlyr(terra::rast(rasti1))))
        rasti2 <-listRaster_RF[which(grepl(yearHi, listRaster_RF, fixed=TRUE) == T)]
        rasti2 <- terra::rast(rasti2, lyrs=c(1:hv_j))
        rasti <- c(rasti1, rasti2)
       
      }
      
      ## 5.3.3 Extract the information for the i-th row ####
      xy <- groundi[, c("longitude", "latitude")]
      xy <- xy %>%
        mutate_if(is.character, as.numeric)
      
      rainfall_points_i <- terra::extract(rasti, xy,method='simple', cells=FALSE)
      
      ## get year
      groundi$Year <- yearPi
      
      # Compute the total amount of rainfall
      groundi$totalRF <- sum(rainfall_points_i[c(2:length(rainfall_points_i))])
      
      # Compute the Number of rainy day
      nrdi <- rainfall_points_i[c(2:length(rainfall_points_i))]
      nrdi[nrdi < 2] <- 0
      nrdi[nrdi >= 2] <- 1
      groundi$nrRainyDays <- sum(nrdi)
      
      # Compute monthly rainfall, at 31 days interval and the remaining  days at the end
      mrdi <- rainfall_points_i[c(2:length(rainfall_points_i))]
      mdiv <- unique(c(seq(1, length(mrdi), 30), length(mrdi)))
      
      mrf <- c()
      for (k in 1:(length(mdiv)-1)) {
        if(k == 1){
          mrf <- c(mrf, sum(mrdi[c(mdiv[k]:mdiv[k+1])]))
        }else{
          mrf <- c(mrf, sum(mrdi[c((mdiv[k]+1):(mdiv[k+1]))]))
        }
      }
      
      if(length(mrf) > 6){## if the crop is > 6 month on the field
        mrf <- c(mrf, rep("NA", 6 -length(mrf)))
      }
      
      mrf_names <- c(paste0("Rain_month", c(1:6)))
      for (h in 1:length(mrf_names)) {
        colname <- mrf_names[h]
        groundi[[colname]] <- mrf[h]
      }
      
      groundi <- subset(groundi, select=-c(Planting, Harvesting, Year))
      rainfall_points <- rbind(rainfall_points, groundi)
    }
  }
  
  rainfall_points <- rainfall_points %>% 
                                select_if(~sum(!is.na(.)) > 0)
  
  
  ## 5.4 Writting of output #### 
  # Check if the directory exists
  Planting_month_date <- gsub("-", "_", Planting_month_date)
  
  fname_rain <- ifelse(AOI == "TRUE", paste("Rainfall_summaries_AOI_", season,"_" ,Planting_month_date, "_", dataSource, ".RDS",sep=""), paste("Rainfall_summaries_trial_", dataSource, ".RDS", sep=""))
  
  saveRDS(object = rainfall_points, file=paste(pathOut1, fname_rain, sep="/"))
  saveRDS(object = rainfall_points, file=paste(pathOut2, fname_rain, sep="/"))
  saveRDS(object = rainfall_points, file=paste(pathOut3, fname_rain, sep="/"))  
  
  return(rainfall_points)
  
}


#################################################################################################################
#################################################################################################################
# 6. Extract the season rainfall parameters on raster data -------------------------------------------
#' @description this functions loops through all .nc files (~30 -40 years) for rainfall to provide raster based seasonal rainfall parameters.
#' @details for AOI it requires a "AOI_GPS.RDS" data frame with c("longitude","latitude") columns being saved in 
#'                            paste("~/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/raw", sep="") 
#'          
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for the entire country
#' @param overwrite default is FALSE 
#' @param Planting_month_date should be provided as month_date
#' @param Harvest_month_date should be provided as month_date
#' @param jobs defines how many cores to use for parallel data sourcing
#' @param season used to formulate a file name and is useful when data for different seasons is needed 
#' @param dataSource is among c("CHIRPS", "AgEra")
#' 
#' @return a raster layer corresponding to the rainfall parameters for each year + if scenario = TRUE, the corresponding 3 scenarios (below, average, above) #' 
#'        totalRF : Total rainfall between pl_Date and hv_Date (mm)
#'        nrRainyDays : Number of rainy days between pl_Date and hv_Date (days)
#'        di : Average daily rainfall between pl_Date and hv_Date (mm/day)
#'        Rain_monthx: total monthly rainfall 
#' @examples: get_rf_rasterSummarydata(country = "Rwanda";  useCaseName = "RAB"; Crop = "Potato"; AOI = FALSE; overwrite = TRUE;
#' season="season1";Planting_month_date = "07-01";  Harvest_month_date = "11-30";jobs=10, dataSource = "CHIRPS", scenario=TRUE)
get_rf_rasterSummarydata <- function(country, useCaseName, Crop, AOI = FALSE, overwrite = FALSE, 
                                    Planting_month_date = "02-01", Harvest_month_date = "05-30", 
                                    jobs = 10, season="season_1", dataSource, scenario=TRUE) {
  
  # 6.1. Initialization of input and output data ####
  
  # Input rainfall
  if(dataSource == "CHIRPS"){
    listRaster_RF <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/Rainfall/chirps", pattern=".nc$", full.names = TRUE)
    listYear_CHIRPS <- seq(1981, (1981+length(listRaster_RF))-1)
  }else{
    listRaster_RF <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/Rainfall/AgEra", pattern=".nc$", full.names = TRUE)
    listYear_CHIRPS <- seq(1981, (1981+length(listRaster_RF))-1)
  }
  
  # Creation of the output dir
  pathOut1 <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/result/Rainfall", sep="")

  if (!dir.exists(pathOut1)){
    dir.create(file.path(pathOut1), recursive = TRUE)
  }
  
 # Input Extent
  if (AOI == FALSE){
    # Case AOI = False
    #read the relevant shape file from gdam to be used to crop the global data
    countryShp <- geodata::gadm(country, level = 3, path='.')
    countryShp <- sf::st_as_sf(countryShp)
  } else {
    # Case AOI = True
    countryCoord <- readRDS(paste("~/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/AOI_GPS.RDS", sep=""))
    countryCoord <- unique(countryCoord[, c("longitude", "latitude")])
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    # get the extent and create a bounding box based on the extent
    x <- list(x=countryCoord$longitude, y=countryCoord$latitude)
    countryShp <- as(raster::extent (x), "SpatialPolygons")
    terra::crs(countryShp) <- "+proj=longlat +datum=WGS84"
    
  }
 
  # Check if both planting and harvest dates are in the same year
  Planting_month <- as.numeric(str_extract(Planting_month_date, "[^-]+"))
  harvest_month <- as.numeric(str_extract(Harvest_month_date, "[^-]+"))
  if(Planting_month < harvest_month){
    planting_harvest_sameYear <- TRUE
  }else{
    planting_harvest_sameYear <- FALSE
  }
  
  # Convert planting date and harvest date in Julian Day 
  pl_Date<-as.Date(paste0("1981-",Planting_month_date), "%Y-%m-%d")
  pl_Date<-as.POSIXlt(pl_Date)$yday
  
  hv_Date <-as.Date(paste0("1981-",Harvest_month_date), "%Y-%m-%d")
  hv_Date<-as.POSIXlt(hv_Date)$yday
  
  # 6.2. Compute the seasonal rainfall parameters for the extent (countryShp) ####
  ## 6.2.1. Case planting and harvesting dates span the same year ####
  if (planting_harvest_sameYear ==  TRUE) {
    
    cls <- makeCluster(jobs)
    doParallel::registerDoParallel(cls)
    
    # Loop over all the years 
    foreach(i=1:length(listRaster_RF), .packages = c('terra', 'plyr', 'stringr','tidyr'), .export='.GlobalEnv') %dopar% {
      
      # Initialize empty raster for the storage
      tot.out <- terra::rast(listRaster_RF[1], lyrs=1)
      tot.out <- terra::crop(tot.out, countryShp)
      tot.out[] <- 'NA'

      # Read raster
      readLayers <- terra::rast(listRaster_RF[i], lyrs=c(pl_Date:hv_Date))
      
      # Crop the raster !!!!! This should be remove and done by Eduardo
      croppedLayers <- terra::crop(readLayers, countryShp)
      
      # Compute the total amount of rainfall
      toti <- terra::app(croppedLayers, fun='sum')
      tot.out <- toti
        
      # Compute the Daily intensity
      dii <- toti/(hv_Date-pl_Date)
      di.out <- dii
        
      # Compute the Number of rainy day
      nrdi <- croppedLayers
      nrdi[croppedLayers < 2] <- 0
      nrdi[croppedLayers >= 2]<-1
      nrdi <- terra::app(nrdi, fun='sum')
      nrd.out <- nrdi
        
      # Compute monthly rainfall, at 31 days interval and the remaining  days at the end
      mrdi <- terra::nlyr(croppedLayers)
      mdiv <- unique(c(seq(1, mrdi, 30), mrdi))
        
        for (k in 1:(length(mdiv)-1)) {
          if(k == 1){
            temp <- croppedLayers[[c(mdiv[k]:mdiv[k+1])]]
            mrf <- terra::app(temp, fun='sum')
          }else{
            temp <- croppedLayers[[c(mdiv[k]+1:mdiv[k+1])]]
            mrf2 <- terra::app(temp, fun='sum')
            mrf <- c(mrf,mrf2)
          }
        }
        
        if(terra::nlyr(mrf) > 6){## if the crop is > 6 month on the field
          mrf <- mrf[[c(1:6)]]
        }
        
      # write ouput for the year i
      rf_summary <- c(tot.out, di.out, nrd.out, mrf)
      names(rf_summary) <- c("TotalRF", "nrRainyDays", "Di", c(paste0("Rain_month", c(1:terra::nlyr(mrf)))))
    
      Planting_month_date2 <- gsub("-", "_", Planting_month_date)
      fname_rain <- ifelse(AOI == "TRUE", paste("Rainfall_Raster_summaries_AOI_", season,"_" ,Planting_month_date2, "_", dataSource,"_", listYear_CHIRPS[i], ".tif",sep=""), paste("Rainfall_Raster_summaries_Country_", season,"_" ,Planting_month_date2, "_", dataSource,"_", listYear_CHIRPS[i],".tif", sep=""))
      terra::writeRaster(rf_summary, paste(pathOut1, fname_rain, sep="/"), filetype="GTiff", overwrite=overwrite)

      }
    stopCluster(cls)
  }
  
  ## 6.2.2. Case planting and harvesting dates span two different year ####
  if (planting_harvest_sameYear ==  FALSE) {
    
    cls <- makeCluster(jobs)
    doParallel::registerDoParallel(cls)
    
    # Loop over all the years 
    foreach(i=1:length(listRaster_RF), .packages = c('terra', 'plyr', 'stringr','tidyr'), .export='.GlobalEnv') %dopar% {
      
      # Initialize empty raster for the storage
      tot.out<-terra::rast(listRaster_RF[1], lyrs=1)
      tot.out<-terra::crop(tot.out, countryShp)
      tot.out[]<-'NA'
      
      # Read raster
      readLayers1<-terra::rast(listRaster_RF[i], lyrs=c(pl_Date:terra::nlyr(terra::rast(listRaster_RF[i]))))
      readLayers2<-terra::rast(listRaster_RF[i+1], lyrs=c(1:hv_Date))
      readLayers <- c(readLayers1, readLayers2)
      
      # Crop the raster !!!!! This should be remove and done by Eduardo
      croppedLayers<-terra::crop(readLayers, countryShp)
      
      # Compute the total amount of rainfall
      toti<-terra::app(croppedLayers, fun='sum')
      tot.out<-toti
      
      # Compute the Daily intensity
      dii<-toti/(hv_Date-pl_Date)
      di.out<-dii
      
      # Compute the Number of rainy day
      nrdi<- croppedLayers
      nrdi[croppedLayers<2] <- 0
      nrdi[croppedLayers>=2]<-1
      nrdi<-terra::app(nrdi, fun='sum')
      nrd.out<- nrdi
      
      # Compute monthly rainfall, at 31 days interval and the remaining  days at the end
      mrdi <- terra::nlyr(croppedLayers)
      mdiv <- unique(c(seq(1, mrdi, 30), mrdi))
      
      for (k in 1:(length(mdiv)-1)) {
        if(k == 1){
          temp <- croppedLayers[[c(mdiv[k]:mdiv[k+1])]]
          mrf <- terra::app(temp, fun='sum')
        }else{
          temp <- croppedLayers[[c(mdiv[k]+1:mdiv[k+1])]]
          mrf2 <- terra::app(temp, fun='sum')
          mrf <- c(mrf,mrf2)
        }
      }
      
      if(terra::nlyr(mrf) > 6){## if the crop is > 6 month on the field
        mrf <- mrf[[c(1:6)]]
      }
      
      # write ouput for the year i
      rf_summary<-c(tot.out,di.out,nrd.out, mrf)
      names(rf_summary) <- c("TotalRF", "nrRainyDays", "Di", c(paste0("Rain_month", c(1:terra::nlyr(mrf)))))
      
      Planting_month_date2 <- gsub("-", "_", Planting_month_date)
      fname_rain <- ifelse(AOI == "TRUE", paste("Rainfall_Raster_summaries_AOI_", season,"_" ,Planting_month_date2, "_", dataSource,"_", listYear_CHIRPS[i], ".tif",sep=""), paste("Rainfall_Raster_summaries_Country_", season,"_" ,Planting_month_date2, "_", dataSource,"_", listYear_CHIRPS[i],".tif", sep=""))
      
      terra::writeRaster(rf_summary, paste(pathOut1, fname_rain, sep="/"), filetype="GTiff", overwrite=overwrite)
      
    }
    stopCluster(cls)
  }
  
  # 6.3. Compute the scenario (below, average, above) of each rainfall parameters ####
  
  # Define the list of summary rasters to use to compute the scenario
  if (AOI == FALSE & scenario == TRUE){
    listRaster_summary <- list.files(path=pathOut1, pattern=paste("Rainfall_Raster_summaries_Country_", season,"_" ,Planting_month_date2, "_", dataSource, sep=""), full.names=T)
  }
  if (AOI == TRUE & scenario == TRUE) {
    listRaster_summary <- list.files(path=pathOut1, pattern=paste("Rainfall_Raster_summaries_AOI_", season,"_" ,Planting_month_date2, "_", dataSource, sep=""), full.names=T)
  }
  
  ## 6.3.1. Loop on all rainfall paramaters ####
  # Define the number of parameters
  n <- terra::nlyr(terra::rast(listRaster_summary[1]))
  
  # Loop
  for (j in 1:n) {
    
    # Open the parameters j for all the years
    listRaster_summaryj <- lapply(listRaster_summary, terra::rast, lyrs=j)
    listRaster_summaryj <- terra::rast(listRaster_summaryj)
    
    # Compute the quantiles
    q <- quantile(listRaster_summaryj, probs=c(0.25,0.5, 0.75))
    
    # Set-up of the output file
    if (j == 1){
      
      namej <- unique(names(listRaster_summaryj))
      below <- q[[1]]
      names(below) <- namej
      average <- q[[2]]
      names(average) <- namej
      above <- q[[3]]
      names(above) <- namej
      
    } else {
      namej <- unique(names(listRaster_summaryj))
      below1 <- q[[1]]
      names(below1) <- namej
      below <- c(below, below1)
      average1 <- q[[2]]
      names(average1) <- namej
      average <- c(average,average1)
      above1 <- q[[3]]
      names(above1) <- namej
      above <- c(above,above1)
    }
    
  }

  ## 6.3.2. Write the output
  fname_scenario <- ifelse(AOI == "TRUE", paste("Rainfall_Raster_scenario_AOI_", season,"_" ,Planting_month_date2, "_", dataSource, ".tif",sep=""), paste("Rainfall_Raster_scenario_Country_", season,"_" ,Planting_month_date2, "_", dataSource,".tif", sep=""))
  
  # Below
  terra::writeRaster(below, paste(pathOut1, paste0("Below_",fname_scenario), sep="/"), filetype="GTiff", overwrite=overwrite)
  # Average
  terra::writeRaster(average, paste(pathOut1, paste0("Average_",fname_scenario), sep="/"), filetype="GTiff", overwrite=overwrite)
  # Above
  terra::writeRaster(above, paste(pathOut1, paste0("Above_",fname_scenario), sep="/"), filetype="GTiff", overwrite=overwrite)

}



