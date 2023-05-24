
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
#' @description a function to crop the WindSpeed global layer but this does duplicate a large volume of data , it is better directly to source from the global data
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param dataSource is one of c("chirts", "AgEra")
#' @param overwrite default is FALSE 
#'
#' @return raster files cropped from global data and the result will be written out in useCaseName/Crop/raw/soil/iSDA
#'
#' @examples get_geoSpatial_soiliSDA(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", dataSource = "chirts", overwrite = TRUE)
crop_geoSpatial_WindSpeed <- function(country, useCaseName, Crop, dataSource, overwrite){
  
  #TODO create a look up table to check use case - country names
  ## get country abbreviation to used in gdam function
  # countryCC <- countrycode(country, origin = 'country.name', destination = 'iso3c')
  
  ## create a directory to store the cropped data: 
  if(dataSource == "chirts"){
    pathOut <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/WindSpeed/chirts", sep="")
    listRaster_WS <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/WindSpeed/chirts", pattern=".nc$")
    listRaster_WS <- terra::rast(paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/WindSpeed/chirts", listRaster_WS, sep="/"))
    fileName <- "/chirts_geospatial_WindSpeed.tif"
  }else{
    pathOut <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/WindSpeed/AgEra", sep="")
    listRaster_WS <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/WindSpeed/AgEra", pattern=".nc$")
    listRaster_WS <- terra::rast(paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/WindSpeed/AgEra", listRaster_WS, sep="/"))
    fileName <- "/AgEra_geospatial_WindSpeed.tif"
  }
  
 
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }
  
  ## read the relevant shape file from gdam to be used to crop the global data
  countryShp <- geodata::gadm(country, level = 3, path='.')
  
  ## crop the layers 
  croppedLayer_WS <- terra::crop(listRaster_WS, countryShp)
  
   ## save result
  terra::writeRaster(croppedLayer_WS, paste(pathOut, fileName , sep="/"), filetype="GTiff", overwrite = overwrite)
  
  return(croppedLayer_WS)
}





#################################################################################################################
#################################################################################################################
#' @description is a function to get average Wind Speed during the growing season, monthly Wind Speed
#' @param rastLayer the .nc file for the planting year, within get_rf_WS_pointdata function, this is provided by the function 
#' @param gpsdata a data frame with longitude and latitude 
#' @param pl_j the planting date as the date of the year
#' @param hv_j the harvest date as the date of the year
#'
#' @return  a data frame with total Wind Speed, number of rainy days and monthly Wind Speed
#' @example summary_pointdata_WS(rastLayer1="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/WindSpeed/AgEra/1992.nc",
                   #' rastLayer2=NULL, gpsdata=data.frame(longitude = c(29.375, 30.125), latitude = c(-2.825, -2.425)),  pl_j=35, hv_j=128,
                   #' planting_harvest_sameYear = TRUE)
summary_pointdata_WS <- function(rastLayer1= NULL, rastLayer2=NULL, gpsdata, pl_j, hv_j, planting_harvest_sameYear){
  if(planting_harvest_sameYear == TRUE){
    PlHvD <- terra::rast(rastLayer1, lyrs=c(pl_j:hv_j))
  }else{
    L1 <- terra::rast(rastLayer1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastLayer1))))
    L2 <- terra::rast(rastLayer2, lyrs=c(1:hv_j))
    PlHvD <- c(L1, L2)
  }
    
    xy <- gpsdata[, c("longitude", "latitude")]
    WSi <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
    WSi <- WSi[,-1]
    
    # Compute the total  and monthly average WS

      gpsdata$AvWindSpeed <- NULL
      for(m in 1:nrow(gpsdata)){
        gdata <- WSi[m, ]
        gpsdata$WindSpeed[m] <- as.numeric(mean(as.numeric(gdata)))
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
        
        mrf_names <- c(paste0("WindSpeed_month", c(1:6)))
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


#' @description this functions loops through all .nc files (~30 -40 years) for WindSpeed to provide point based data.
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
#' @param season used to formulate a file name and is useful when data for different seasons is needed 
#' @param dataSource is among c("chirts", "AgEra")
#' @param ID only when AOI  = FALSE, it is the column name Identifying the trial ID in compiled_fieldData.RDS
#' 
#' 
#' @return a data frame containing the col information & columns corresponding to the WindSpeed parameters#' 
#'        AvWindSpeed : average WindSpeed between pl_Date and hv_Date
#'        monthly_: monthly Wind Speed for the growing season
#' @examples: get_WindSpeed_pointSummarydata(country = "Rwanda";  useCaseName = "RAB"; Crop = "Potato"; AOI = FALSE; overwrite = TRUE;
#' season="season1";Planting_month_date = "07-01";  Harvest_month_date = "11-30"; jobs=10, ID = "TLID")
get_WindSpeed_pointSummarydata <- function(country, useCaseName, Crop, AOI = FALSE, overwrite = FALSE, 
                                         Planting_month_date = "02-01", Harvest_month_date = "05-30", 
                                         jobs = 10, season=NULL, dataSource, ID){
  
  ## define the directories store the result and also read list of .nc files 
  if(dataSource == "chirts"){
    listRaster_WS <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/WindSpeed/chirts", pattern=".nc$", full.names = TRUE)
  }else if(dataSource == "AgEra"){
    listRaster_WS <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/WindSpeed/AgEra", pattern=".nc$", full.names = TRUE)
  }
  
  pathOut1 <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/result/WindSpeed", sep="")
  pathOut2 <- paste("/home/jovyan/agwise/AgWise_Data/potential_yield/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/WindSpeed", sep="")
  pathOut3 <- paste("/home/jovyan/agwise/AgWise_Data/response_functions/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/WindSpeed", sep="")
  
  
  
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
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
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
      WindSpeed_result <- foreach(i=1:length(listRaster_WS), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        rast1 <- listRaster_WS[i]
        source("~/agwise/AgWise_Scripts/data_sourcing/get_geoSpatialWindSpeed_MC.R", local = TRUE)
        summary_pointdata_WS(rastLayer1=rast1, rastLayer2=NULL, gpsdata = ground, pl_j=pl_j, hv_j=hv_j, planting_harvest_sameYear = TRUE)
      }
      WindSpeed_points <- do.call(rbind, WindSpeed_result)
      
      stopCluster(cls)
    }
    
    if (planting_harvest_sameYear ==  FALSE) {
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      
      ## WindSpeed
      WS_result2 <- foreach(i = 1:(length(listRaster_WS)-1), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        listRaster_WS <- listRaster_WS[order(listRaster_WS)]
        source("~/agwise/AgWise_Scripts/data_sourcing/get_geoSpatialWindSpeed_MC.R", local = TRUE)
        rast1 <- listRaster_WS[i]
        rast2 <- listRaster_WS[i+1]
        summary_pointdata_WS(rastLayer1=rast1, rastLayer2=rast2, gpsdata = ground, pl_j=pl_j, hv_j=hv_j, planting_harvest_sameYear = TRUE)
      }
      WindSpeed_points <- do.call(rbind, WS_result2)
      
      stopCluster(cls)
      
    }
    
  }else {## when the planting and harvest dates varies for every row of data because it is actual trial data
    
    
    # 3 Loop on all the ID to calculate the parameters ####
    WindSpeed_points <- NULL
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
        ## WindSpeed
        WSi <-listRaster_WS[which(grepl(yearPi, listRaster_WS, fixed=TRUE) == T)]
        WSi <- terra::rast(WSi, lyrs=c(pl_j:hv_j))
      }
      
      ## 3.2 Case two years ####
      if (yearPi < yearHi) {
        
        ### 3.2.2 Read for the corresponding years and date ####
        ## WindSpeed
        WSi1<-listRaster_WS[which(grepl(yearPi, listRaster_WS, fixed=TRUE) == T)]
        WSi1 <- terra::rast(WSi1, lyrs=c(pl_j:terra::nlyr(terra::rast(WSi1))))
        WSi2 <-listRaster_WS[which(grepl(yearHi, listRaster_WS, fixed=TRUE) == T)]
        WSi2 <- terra::rast(WSi2, lyrs=c(1:hv_j))
        WSi <- c(WSi1, WSi2)
       
      }
      
      ### 3.3 Extract the information for the i-th row ####
      xy <- groundi[, c("longitude", "latitude")]
      xy <- xy %>%
        mutate_if(is.character, as.numeric)
      
      WindSpeed_points_i <- terra::extract(WSi, xy,method='simple', cells=FALSE)
      WindSpeed_points_i <- WindSpeed_points_i[, -1]
      
      ## get year
      groundi$Year <- yearPi
      
      # Compute the total amount of WindSpeed
      groundi$AvWindSpeed <- round(mean(as.numeric(WindSpeed_points_i[c(1:length(WindSpeed_points_i))])), digits=1)
      
    # Compute monthly WindSpeed, at 31 days interval and the remaining  days at the end
     mdiv <- unique(c(seq(1, length(WindSpeed_points_i), 30), length(WindSpeed_points_i)))
      
      mrf <- c()
      for (k in 1:(length(mdiv)-1)) {
        if(k == 1){
          mrf <- c(mrf, mean(as.numeric(WindSpeed_points_i[c(mdiv[k]:mdiv[k+1])])))
        }else{
          mrf <- c(mrf, mean(as.numeric(WindSpeed_points_i[c((mdiv[k]+1):(mdiv[k+1]))])))
        }
      }
      
      if(length(mrf) > 6){## if the crop is > 6 month on the field
        mrf <- c(mrf, rep("NA", 6 -length(mrf)))
      }
      
      mrf_names <- c(paste0("WindSpeed_month", c(1:6)))
      for (h in 1:length(mrf_names)) {
        colname <- mrf_names[h]
        groundi[[colname]] <- mrf[h]
      }
      
      groundi <- subset(groundi, select=-c(Planting, Harvesting, Year))
      WindSpeed_points <- rbind(WindSpeed_points, groundi)
    }
  }
  
  WindSpeed_points <- WindSpeed_points %>% 
                                select_if(~sum(!is.na(.)) > 0)
  
  
  # 4 Writting of output: Check if the directory exists
  Planting_month_date <- gsub("-", "_", Planting_month_date)
  
  fname_WS <- ifelse(AOI == "TRUE", paste("WindSpeed_summaries_AOI_", season,"_" ,Planting_month_date, "_", dataSource, ".RDS",sep=""), paste("WindSpeed_summaries_trial_", dataSource, ".RDS", sep=""))
 
  saveRDS(object = WindSpeed_points, file=paste(pathOut1, fname_WS, sep="/"))
  saveRDS(object = WindSpeed_points, file=paste(pathOut2, fname_WS, sep="/"))
  saveRDS(object = WindSpeed_points, file=paste(pathOut3, fname_WS, sep="/"))  
  
  return(WindSpeed_points)
  
}







#################################################################################################################
#################################################################################################################


#' @description this functions loops through all .nc files (~30 - 40 years) for Wind Speed and provide point based data.
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
#' @param season used to formulate a file name and is useful when data for different seasons is needed 
#' @param dataSource is one of c("chirts", "AgEra")
#' @param ID only when AOI  = FALSE, it is the column name Identifying the trial ID in compiled_fieldData.RDS
#' 
#' 
#' @return a data frame with file name made to reflect point/summary, AOI/trial, season, planting as mm_dd and source chirts/AgEra
#' the data frame contains daily Wind Speed for every GPS point. 
#' For the trial sites: it provides:longitude, latitude, plantingDate and harvestDate as yyyy-mm-dd, yearPi & yearHi as yyyy, 
#' pl_j & hv_j as date of the year for panting & harvest, growinglength and daily Wind Speed by date, NA for dates not part of the growing season for a trial
#' For AOI: it provides daily Wind Speed for the dates between Planting_month_date and Harvest_month_dates plus 
#' plantingYear, harvestYear, longitude, latitude   
#' 
#' @examples: get_WindSpeed_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE, overwrite = TRUE,
#'             Planting_month_date = "07-01",  Harvest_month_date = "11-30", 
#'             jobs=10, season="season_1", dataSource = "AgEra", ID = "TLID")
get_WindSpeed_pointData <- function(country, useCaseName, Crop, AOI = FALSE, overwrite = FALSE, 
                               Planting_month_date = "02-01", Harvest_month_date = "05-30", 
                               jobs = 10, season="season_1", dataSource, ID = NULL){
  
  if(dataSource == "chirts"){
    listRaster_WS <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/WindSpeed/chirts", pattern=".nc$", full.names = TRUE)
  }else if(dataSource == "AgEra"){
    listRaster_WS <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/WindSpeed/AgEra", pattern=".nc$", full.names = TRUE)
  }
  
  pathOut1 <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/result/WindSpeed", sep="")
  pathOut2 <- paste("/home/jovyan/agwise/AgWise_Data/potential_yield/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/WindSpeed", sep="")
  pathOut3 <- paste("/home/jovyan/agwise/AgWise_Data/response_functions/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/WindSpeed", sep="")
  
  
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
  
  
  if(AOI == TRUE){
    
    ### 3.1.1 Convert planting Date and harvesting in Julian Day ####
    pl_j <-as.POSIXlt(unique(ground$Planting))$yday
    hv_j <-as.POSIXlt(unique(ground$Harvesting))$yday
    
    if (planting_harvest_sameYear ==  TRUE) {
      
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      
      ### 3.1.2 Read for the corresponding year and date. Avoid the last year because it is not complete for 365 days
      WS_result <- foreach(i=1:(length(listRaster_WS)-1), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        rasti <- listRaster_WS[i]
        PlHvD <- terra::rast(rasti, lyrs=c(pl_j:hv_j))
        xy <- ground[, c("longitude", "latitude")]
        WSi <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
        WSi <- WSi[,-1]
        names(WSi) <- paste("WS", sub("^[^_]+", "", names(WSi)), sep="")
        WSi$plantingYear <-  str_extract(rasti, "[[:digit:]]+")
        WSi$harvestYear <- str_extract(rasti, "[[:digit:]]+")
        ground2 <- cbind(ground, WSi)
      }
      WindSpeed_points <- do.call(rbind, WS_result)
      
      stopCluster(cls)
    }
    
    
    if (planting_harvest_sameYear ==  FALSE) {
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      ## WindSpeed
      WS_result2 <- foreach(i = 1:(length(listRaster_WS)-1), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        listRaster_WS <- listRaster_WS[order(listRaster_WS)]
        rast1 <- listRaster_WS[i]
        rast2 <- listRaster_WS[i+1]
        rasti1 <- terra::rast(rast1, lyrs=c(pl_j:terra::nlyr(terra::rast(rast1))))
        rasti2 <- terra::rast(rast2, lyrs=c(1:hv_j))
        PlHvD <- c(rasti1, rasti2)
        xy <- ground[, c("longitude", "latitude")]
        WSi <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
        WSi <- WSi[,-1]
        
        names(WSi) <- sub("^[^_]+", "", names(WSi))
        if(length(grep("_366", names(WSi))) > 0){
          WSi <- WSi[,-grep("_366", names(WSi))]
        }
        names(WSi) <- paste("WS", names(WSi), sep="")
        WSi$plantingYear <- str_extract(rast1, "[[:digit:]]+")
        WSi$harvestYear <- str_extract(rast2, "[[:digit:]]+")
        ground2 <- cbind(ground, WSi)
      }
      
      WindSpeed_points <- do.call(rbind, WS_result2)
      
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
    WS_names <- c(paste0("WS_", c(min(ground$pl_j):max(ground$hv_j))))
    WS_names2 <-  as.data.frame(matrix(nrow=length(WS_names), ncol=1))
    colnames(WS_names2) <- "ID"
    WS_names2$ID <- c(1:nrow(WS_names2))
    WS_names2[,2] <- WS_names
    
    
    # 3 Loop on all the ID to calculate the parameters ####
    WindSpeed_points <- NULL
    for(i in 1:nrow(ground)){
      print(i)
      # Extract the information for the i-th row
      groundi <- ground[i, c("longitude", "latitude", "plantingDate", "harvestDate",ID,"NAME_1", "NAME_2","yearPi", "yearHi", "pl_j", "hv_j", "growinglength")]
      
      # Test if the cropping season overlaps two civil year
      yearPi <- groundi$yearPi
      yearHi <- groundi$yearHi
      ### 3.2.1 Convert planting Date and harvesting in Julian Day ####
      pl_j <- groundi$pl_j
      hv_j <- groundi$hv_j
      
      
      ## 3.1 Case same year ####
      if (yearPi == yearHi) {
        rasti<-listRaster_WS[which(grepl(yearPi, listRaster_WS, fixed=TRUE) == T)]
        rasti <- terra::rast(rasti, lyrs=c(pl_j:hv_j))
      }
      
      ## 3.2 Case two years ####
      if (yearPi < yearHi) {
        rasti1<-listRaster_WS[which(grepl(yearPi, listRaster_WS, fixed=TRUE) == T)]
        rasti1 <- terra::rast(rasti1, lyrs=c(pl_j:terra::nlyr(terra::rast(rasti1))))
        rasti2 <-listRaster_WS[which(grepl(yearHi, listRaster_WS, fixed=TRUE) == T)]
        rasti2 <- terra::rast(rasti2, lyrs=c(1:hv_j))
        rasti <- c(rasti1, rasti2)
        
      }
      
      ### 3.3 Extract the information for the i-th row ####
      xy <- groundi[, c("longitude", "latitude")]
      xy <- xy %>%
        mutate_if(is.character, as.numeric)
      
      WSi <- terra::extract(rasti, xy,method='simple', cells=FALSE)
      WSi <- WSi[,-1]
      names(WSi) <- sub("^[^_]+", "", names(WSi))
      names(WSi) <- paste("WS", names(WSi), sep="")
      WSi <- as.data.frame(t(WSi))
      WSi$V2 <- rownames(WSi)
      rownames(WSi) <- NULL
      
      WSi <- merge(WSi,WS_names2, by="V2", all.y = TRUE)
      WSi <- WSi[order(WSi$ID),]
      WSi <- WSi[,-3]
      WSi2 <- as.data.frame(t(WSi))
      colnames(WSi2) <- WSi$V2
      WSi2 <- WSi2[-1,]
      WSi2 <- cbind(groundi, WSi2)
      WindSpeed_points <- rbind(WindSpeed_points, WSi2)
    }
  }
  
  
  # 4 Writting of output: Check if the directory exists
  Planting_month_date <- gsub("-", "_", Planting_month_date)
  
  
  fname_WS <- ifelse(AOI == "TRUE", paste("WindSpeed_pointData_AOI_", season,"_" ,Planting_month_date, "_", dataSource, ".RDS",sep=""), paste("WindSpeed_pointData_trial_", dataSource, ".RDS", sep=""))
  
  saveRDS(object = WindSpeed_points, file=paste(pathOut1, fname_WS, sep="/"))
  saveRDS(object = WindSpeed_points, file=paste(pathOut2, fname_WS, sep="/"))
  saveRDS(object = WindSpeed_points, file=paste(pathOut3, fname_WS, sep="/")) 
  return(WindSpeed_points)
  
}



