
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
#' @description a function to crop the TemperatureMax global layer but this does duplicate a large volume of data , it is better directly to source from the global data
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param dataSource is one of c("chirts", "AgEra")
#' @param overwrite default is FALSE 
#'
#' @return raster files cropped from global data and the result will be written out in useCaseName/Crop/raw/soil/iSDA
#'
#' @examples get_geoSpatial_soiliSDA(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", dataSource = "chirts", overwrite = TRUE)
crop_geoSpatial_temp <- function(country, useCaseName, Crop, dataSource, overwrite){
  
  ## create a directory to store the cropped data: 
  if(dataSource == "chirts"){
    pathOut <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/Temperature/chirts", sep="")
    ## read soil layers and crop
    listRaster_tmax <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMax/chirts", pattern=".nc$")
    readLayers_tmax <- terra::rast(paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMax/chirts", listRaster_tmax, sep="/"))
    fileName <- "/chirts_geospatial_TemperatureMax.tif"
  }else{
    pathOut <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/TemperatureMax/AgEra", sep="")
    listRaster_tmax <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMax/AgEra", pattern=".nc$")
    readLayers_tmax <- terra::rast(paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMax/AgEra", listRaster_tmax, sep="/"))
    fileName <- "/AgEra_geospatial_TemperatureMax.tif"
  }
  
 
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }
  
  ## read the relevant shape file from gdam to be used to crop the global data
  countryShp <- geodata::gadm(country, level = 3, path='.')
  
  ## crop the layers 
  croppedLayer_tmax <- terra::crop(readLayers_tmax, countryShp)
  
   ## save result
  terra::writeRaster(croppedLayer_tmax, paste(pathOut, fileName , sep="/"), filetype="GTiff", overwrite = overwrite)
  
  return(croppedLayer_tmax)
}





#################################################################################################################
#################################################################################################################
#' @description is a function to get total TemperatureMax, number of rainy days and monthly temperature when the planting and harvest happen in the same years
#' @param rastLayer the .nc file for the planting year, within get_rf_temp_pointdata function, this is provided by the function 
#' @param gpsdata a data frame with longitude and latitude 
#' @param pl_j the planting date as the date of the year
#' @param hv_j the harvest date as the date of the year
#' @param varName is used to rename the column names it assumes one the following values c("Rain", "Tmax", "Tmin")
#'
#' @return  a data frame with total TemperatureMax, number of rainy days and monthly temperature
#' @example sameYear_pointdata(rastLayer="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMax/chirts/1981.nc",
                   #' gpsdata=data.frame(longitude = c(29.375, 30.125), latitude = c(-2.825, -2.425)),  pl_j=35, hv_j=128)
summary_pointdata_temp <- function(rastLayer1= NULL, rastLayer2=NULL, gpsdata, pl_j, hv_j, planting_harvest_sameYear, varName){
  if(planting_harvest_sameYear == TRUE){
    PlHvD <- terra::rast(rastLayer1, lyrs=c(pl_j:hv_j))
  }else{
    tmxi1 <- terra::rast(rastLayer1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastLayer1))))
    tmxi2 <- terra::rast(rastLayer2, lyrs=c(1:hv_j))
    PlHvD <- c(tmxi1, tmxi2)
  }
    
    xy <- gpsdata[, c("longitude", "latitude")]
    tempi <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
    tempi <- tempi[,-1]
    tempi <- tempi-274
    # Compute the total  and monthly average temp 

      # tmxiq <- t(tempi[c(1:length(tempi))])
      gpsdata$AvTemp <- NULL
      
      for(m in 1:nrow(gpsdata)){
        gdata <- tempi[m, ]
        
        gpsdata$AvTemp[m] <- as.numeric(mean(as.numeric(gdata)))
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
        
        if(varName == "Tmax"){
          mrf_names <- c(paste0("Tmax_month", c(1:6)))
        }else if(varName == "Tmin"){
          mrf_names <- c(paste0("Tmin_month", c(1:6)))
        }
        
       
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
      
      
    
      if(varName == "Tmax"){
        gpsdata$Tmax_mean <- gpsdata$AvTemp
      }else if(varName == "Tmin"){
        gpsdata$Tmin_mean <- gpsdata$AvTemp
      }
      
      gpsdata <- subset(gpsdata, select = -c(AvTemp))
        
    # names(tmxi) <- paste(varName, sub("^[^_]+", "", names(tmxi)), sep="")
    # tmxi$Year <- str_extract(rastLayer, "[[:digit:]]+")
    # tmxi <- cbind(tmxi, xy)
     return(gpsdata)
}





#################################################################################################################
#################################################################################################################


#' @description this functions loops through all .nc files (~30 -40 years) for TemperatureMax, and min and max temp to provide point based data.
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
#' @param varName is used to rename the column names it assumes one the following values c("Tmax", "Tmin")
#' 
#' @return a data frame containing the col information & columns corresponding to the TemperatureMax parameters#' 
#'        totalRF : Total TemperatureMax between pl_Date and hv_Date (mm)
#'        nrRainyDays : Number of rainy days between pl_Date and hv_Date (days)
#'        di : Average daily TemperatureMax between pl_Date and hv_Date (mm/day)
#'        tmin : Average tmin temperature between pl_Date and hv_Date 
#'        tmax : Average tmax temperature between pl_Date and hv_Date
#'        monthlyRF_x: total monthly TemperatureMax 
#' @examples: get_temp_pointSummarydata(country = "Rwanda";  useCaseName = "RAB"; Crop = "Potato"; AOI = FALSE; overwrite = TRUE;
#' Planting_month_date = "07-01";  Harvest_month_date = "11-30"; jobs=10, varName = "Tmax", ID = "TLID")
get_temp_pointSummarydata <- function(country, useCaseName, Crop, AOI = FALSE, overwrite = FALSE, 
                                         Planting_month_date = "02-01", Harvest_month_date = "05-30", 
                                         jobs = 10, dataSource, varName, ID){
  
  
 
  ## define the directories store the result and also read list of .nc files 
  if(dataSource == "chirts" & varName == "Tmax"){
    listRaster_temp <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMax/chirts", pattern=".nc$", full.names = TRUE)
  }else if(dataSource == "AgEra" & varName == "Tmax"){
    listRaster_temp <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMax/AgEra", pattern=".nc$", full.names = TRUE)
  }else if(dataSource == "chirts" & varName == "Tmin"){
    listRaster_temp <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMin/chirts", pattern=".nc$", full.names = TRUE)
  }else if(dataSource == "AgEra" & varName == "Tmin"){
    listRaster_temp <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMin/AgEra", pattern=".nc$", full.names = TRUE)
  }
  
  pathOut1 <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/result/Temperature", sep="")
  pathOut2 <- paste("/home/jovyan/agwise/AgWise_Data/potential_yield/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/Temperature", sep="")
  pathOut3 <- paste("/home/jovyan/agwise/AgWise_Data/response_functions/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/Temperature", sep="")
  
  
  
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
    countryCoord <- unique(GPS_fieldData[, c("lon", "lat", "plantingDate", "harvestDate",ID)])
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    names(countryCoord) <- c("longitude", "latitude", "plantingDate", "harvestDate",ID)
    countryCoord <- countryCoord[complete.cases(countryCoord),]
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
      t_result <- foreach(i=1:length(listRaster_temp), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        rast1 <- listRaster_temp[i]
        source("~/agwise/AgWise_Scripts/data_sourcing/get_geoSpatialTemperature_MC.R", local = TRUE)
        summary_pointdata_temp(rastLayer1=rast1, rastLayer2=NULL, gpsdata = ground, pl_j=pl_j, hv_j=hv_j, 
                               planting_harvest_sameYear = TRUE, varName = varName)
      }
      Temperature_points <- do.call(rbind, t_result)
      
      stopCluster(cls)
    }
    
    if (planting_harvest_sameYear ==  FALSE) {
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      
      ## TemperatureMax
      t_result2 <- foreach(i = 1:(length(listRaster_temp)-1), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        listRaster_temp <- listRaster_temp[order(listRaster_temp)]
        source("~/agwise/AgWise_Scripts/data_sourcing/get_rain_temp_summary.R", local = TRUE)
        rast1 <- listRaster_temp[i]
        rast2 <- listRaster_temp[i+1]
        summary_pointdata_temp(rastLayer1=rast1, rastLayer2=rast2, gpsdata = ground, pl_j=pl_j, hv_j=hv_j, 
                               planting_harvest_sameYear = TRUE, varName = varName)
      }
      Temperature_points <- do.call(rbind, t_result2)
      
      stopCluster(cls)
      
    }
    
  }else {## when the planting and harvest dates varies for every row of data because it is actual trial data
    
    
    # 3 Loop on all the ID to calculate the parameters ####
    Temperature_points <- NULL
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
        ## TemperatureMax
        tempi<-listRaster_temp[which(grepl(yearPi, listRaster_temp, fixed=TRUE) == T)]
        tempi <- terra::rast(tempi, lyrs=c(pl_j:hv_j))
      }
      
      ## 3.2 Case two years ####
      if (yearPi < yearHi) {
        
        ### 3.2.2 Read for the corresponding years and date ####
        ## TemperatureMax
        tempi1<-listRaster_temp[which(grepl(yearPi, listRaster_temp, fixed=TRUE) == T)]
        tempi1 <- terra::rast(tempi1, lyrs=c(pl_j:terra::nlyr(terra::rast(tempi1))))
        tempi2 <-listRaster_temp[which(grepl(yearHi, listRaster_temp, fixed=TRUE) == T)]
        tempi2 <- terra::rast(tempi2, lyrs=c(1:hv_j))
        tempi <- c(tempi1, tempi2)
       
      }
      
      ### 3.3 Extract the information for the i-th row ####
      xy <- groundi[, c("longitude", "latitude")]
      xy <- xy %>%
        mutate_if(is.character, as.numeric)
      
      Temperature_points_i <- terra::extract(tempi, xy,method='simple', cells=FALSE)
      Temperature_points_i <- Temperature_points_i[, -1]
      Temperature_points_i <- Temperature_points_i - 274
      ## get year
      groundi$Year <- yearPi
      
      # Compute the total amount of Temperature
      if(varName == "Tmax"){
        groundi$Tmax_mean <- round(mean(as.numeric(Temperature_points_i[c(1:length(Temperature_points_i))])), digits=1)
      }else if(varName == "Tmin"){
        groundi$Tmin_mean <- round(mean(as.numeric(Temperature_points_i[c(1:length(Temperature_points_i))])), digits=1)
      }
      
      
    # Compute monthly Temperature, at 31 days interval and the remaining  days at the end
     mdiv <- unique(c(seq(1, length(Temperature_points_i), 30), length(Temperature_points_i)))
      
      mrf <- c()
      for (k in 1:(length(mdiv)-1)) {
        if(k == 1){
          mrf <- c(mrf, mean(as.numeric(Temperature_points_i[c(mdiv[k]:mdiv[k+1])])))
        }else{
          mrf <- c(mrf, mean(as.numeric(Temperature_points_i[c((mdiv[k]+1):(mdiv[k+1]))])))
        }
      }
      
      if(length(mrf) > 6){## if the crop is > 6 month on the field
        mrf <- c(mrf, rep("NA", 6 -length(mrf)))
      }
      
      if(varName == "Tmax"){
        mrf_names <- c(paste0("Tmax_month", c(1:6)))
      }else if(varName == "Tmin"){
        mrf_names <- c(paste0("Tmin_month", c(1:6)))
      }
      
      for (h in 1:length(mrf_names)) {
        colname <- mrf_names[h]
        groundi[[colname]] <- mrf[h]
      }
      
      groundi <- subset(groundi, select=-c(Planting, Harvesting, Year))
      Temperature_points <- rbind(Temperature_points, groundi)
    }
  }
  
  Temperature_points <- Temperature_points %>% 
                                select_if(~sum(!is.na(.)) > 0)
  
  
  # 4 Writting of output: Check if the directory exists
  Planting_month_date <- gsub("-", "_", Planting_month_date)
  
  fname_Temp <- ifelse(AOI == "TRUE", paste(varName, "_summaries_AOI_" ,Planting_month_date, "_", dataSource, ".RDS",sep=""), paste(varName, "_summaries_trial_", dataSource, ".RDS", sep=""))
 
  saveRDS(object = Temperature_points, file=paste(pathOut1, fname_Temp, sep="/"))
  saveRDS(object = Temperature_points, file=paste(pathOut2, fname_Temp, sep="/"))
  saveRDS(object = Temperature_points, file=paste(pathOut3, fname_Temp, sep="/"))  
  
  return(Temperature_points)
  
}







#################################################################################################################
#################################################################################################################


#' @description this functions loops through all .nc files (~30 - 40 years) for TemperatureMax, and min and max temp to provide point based data.
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
#' @param varName is used to rename the column names it assumes one the following values c("Tmax", "Tmin")
#' @param ID only when AOI  = FALSE, it is the column name Identifying the trial ID in compiled_fieldData.RDS
#' 
#' @return a data frame with file name made to reflect point/summary, AOI/trial, season, planting as mm_dd and source chirts/AgEra
#' the data frame contains daily TemperatureMax for every GPS point. 
#' For the trial sites: it provides:longitude, latitude, plantingDate and harvestDate as yyyy-mm-dd, yearPi & yearHi as yyyy, 
#' pl_j & hv_j as date of the year for panting & harvest, growinglength and daily TemperatureMax by date, NA for dates not part of the growing season for a trial
#' For AOI: it provides dily TemperatureMax for ll dates between Planting_month_date and Harvest_month_dates plus 
#' plantingYear, harvestYear, longitude, latitude   
#' 
#' @examples: get_temp_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE, overwrite = TRUE,
#'             Planting_month_date = "07-01",  Harvest_month_date = "11-30", 
#'             jobs=10, dataSource = "AgEra", varName = "Tmax")
get_temp_pointData <- function(country, useCaseName, Crop, AOI = FALSE, overwrite = FALSE, 
                               Planting_month_date = "02-01", Harvest_month_date = "05-30", 
                               jobs = 10, dataSource, varName = NULL, ID = NULL){
  
  if(dataSource == "chirts" & varName == "Tmax"){
    listRaster_temp <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMax/chirts", pattern=".nc$", full.names = TRUE)
  }else if(dataSource == "AgEra" & varName == "Tmax"){
    listRaster_temp <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMax/AgEra", pattern=".nc$", full.names = TRUE)
  }else if(dataSource == "chirts" & varName == "Tmin"){
    listRaster_temp <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMin/chirts", pattern=".nc$", full.names = TRUE)
  }else if(dataSource == "AgEra" & varName == "Tmin"){
    listRaster_temp <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMin/AgEra", pattern=".nc$", full.names = TRUE)
  }
  
  pathOut1 <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/result/Temperature", sep="")
  pathOut2 <- paste("/home/jovyan/agwise/AgWise_Data/potential_yield/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/Temperature", sep="")
  pathOut3 <- paste("/home/jovyan/agwise/AgWise_Data/response_functions/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/Temperature", sep="")
  
  
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
      
      ### 3.1.2 Read for the corresponding year and date. Avoid the last year because it is not complete for 365 days
      temp_result <- foreach(i=1:(length(listRaster_temp)-1), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        rasti <- listRaster_temp[i]
        PlHvD <- terra::rast(rasti, lyrs=c(pl_j:hv_j))
        xy <- ground[, c("longitude", "latitude")]
        tempi <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
        tempi <- tempi[,-1]
        tempi <- tempi-274
        names(tempi) <- paste("Temp", sub("^[^_]+", "", names(tempi)), sep="")
        tempi$plantingYear <-  str_extract(rasti, "[[:digit:]]+")
        tempi$harvestYear <- str_extract(rasti, "[[:digit:]]+")
        ground2 <- cbind(ground, tempi)
      }
      Temperature_points <- do.call(rbind, temp_result)
      
      stopCluster(cls)
    }
    
    
    if (planting_harvest_sameYear ==  FALSE) {
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      ## TemperatureMax
      temp_result2 <- foreach(i = 1:(length(listRaster_temp)-1), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        listRaster_temp <- listRaster_temp[order(listRaster_temp)]
        rast1 <- listRaster_temp[i]
        rast2 <- listRaster_temp[i+1]
        rasti1 <- terra::rast(rast1, lyrs=c(pl_j:terra::nlyr(terra::rast(rast1))))
        rasti2 <- terra::rast(rast2, lyrs=c(1:hv_j))
        PlHvD <- c(rasti1, rasti2)
        xy <- ground[, c("longitude", "latitude")]
        tempi <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
        tempi <- tempi[,-1]
        tempi <- tempi-274
        names(tempi) <- sub("^[^_]+", "", names(tempi))
        if(length(grep("_366", names(tempi))) > 0){
          tempi <- tempi[,-grep("_366", names(tempi))]
        }
        names(tempi) <- paste("Temp", names(tempi), sep="")
        tempi$plantingYear <- str_extract(rast1, "[[:digit:]]+")
        tempi$harvestYear <- str_extract(rast2, "[[:digit:]]+")
        ground2 <- cbind(ground, tempi)
      }
      
      Temperature_points <- do.call(rbind, temp_result2)
      
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
    Temp_names <- c(paste0("Temp_", c(min(ground$pl_j):max(ground$hv_j))))
    Temp_names2 <-  as.data.frame(matrix(nrow=length(Temp_names), ncol=1))
    colnames(Temp_names2) <- "ID"
    Temp_names2$ID <- c(1:nrow(Temp_names2))
    Temp_names2[,2] <- Temp_names
    
    
    # 3 Loop on all the ID to calculate the parameters ####
    Temperature_points <- NULL
    for(i in 1:nrow(ground)){
      print(i)
      # Extract the information for the i-th row
      # groundi <- ground[i, c("longitude", "latitude", "plantingDate", "harvestDate","yearPi", "yearHi", "pl_j", "hv_j", "growinglength")]
      groundi <- ground[i, c("longitude", "latitude", "plantingDate", "harvestDate",ID,"NAME_1", "NAME_2","yearPi", "yearHi", "pl_j", "hv_j", "growinglength")]
      
      # Test if the cropping season overlaps two civil year
      yearPi <- groundi$yearPi
      yearHi <- groundi$yearHi
      ### 3.2.1 Convert planting Date and harvesting in Julian Day ####
      pl_j <- groundi$pl_j
      hv_j <- groundi$hv_j
      
      
      ## 3.1 Case same year ####
      if (yearPi == yearHi) {
        rasti<-listRaster_temp[which(grepl(yearPi, listRaster_temp, fixed=TRUE) == T)]
        rasti <- terra::rast(rasti, lyrs=c(pl_j:hv_j))
      }
      
      ## 3.2 Case two years ####
      if (yearPi < yearHi) {
        rasti1<-listRaster_temp[which(grepl(yearPi, listRaster_temp, fixed=TRUE) == T)]
        rasti1 <- terra::rast(rasti1, lyrs=c(pl_j:terra::nlyr(terra::rast(rasti1))))
        rasti2 <-listRaster_temp[which(grepl(yearHi, listRaster_temp, fixed=TRUE) == T)]
        rasti2 <- terra::rast(rasti2, lyrs=c(1:hv_j))
        rasti <- c(rasti1, rasti2)
        
      }
      
      ### 3.3 Extract the information for the i-th row ####
      xy <- groundi[, c("longitude", "latitude")]
      xy <- xy %>%
        mutate_if(is.character, as.numeric)
      
      tempi <- terra::extract(rasti, xy,method='simple', cells=FALSE)
      tempi <- tempi[,-1]
      names(tempi) <- sub("^[^_]+", "", names(tempi))
      names(tempi) <- paste("Temp", names(tempi), sep="")
      tempi <- as.data.frame(t(tempi))
      tempi$V1 <- tempi$V1 - 274
      tempi$V2 <- rownames(tempi)
      rownames(tempi) <- NULL
      
      tempi <- merge(tempi,Temp_names2, by="V2", all.y = TRUE)
      tempi <- tempi[order(tempi$ID),]
      tempi <- tempi[,-3]
      tempi2 <- as.data.frame(t(tempi))
      colnames(tempi2) <- tempi$V2
      tempi2 <- tempi2[-1,]
      tempi2 <- cbind(groundi, tempi2)
      Temperature_points <- rbind(Temperature_points, tempi2)
    }
  }
  
  
  # 4 Writting of output: Check if the directory exists
  Planting_month_date <- gsub("-", "_", Planting_month_date)
  
  
  fname_temp <- ifelse(AOI == "TRUE", paste(varName, "_pointData_AOI_",Planting_month_date, "_", dataSource, ".RDS",sep=""), paste(varName,"_pointData_trial_", dataSource, ".RDS", sep=""))
  
  saveRDS(object = Temperature_points, file=paste(pathOut1, fname_temp, sep="/"))
  saveRDS(object = Temperature_points, file=paste(pathOut2, fname_temp, sep="/"))
  saveRDS(object = Temperature_points, file=paste(pathOut3, fname_temp, sep="/")) 
  return(Temperature_points)
  
}





#################################################################################################################
#################################################################################################################
# 6. Extract the seasonal temperature on raster data -------------------------------------------
#' @description this functions loops through all .nc files (~30 -40 years) for temperature to provide raster based seasonal temperature.
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
#' @param dataSource is among c("chirst", "AgEra")
#' 
#' @return a raster layer corresponding to the temperature (Min and Max) for each year + if scenario = TRUE, the corresponding 3 scenarios (below, average, above) #' 
#'        tmin : Average tmin temperature between pl_Date and hv_Date 
#'        tmax : Average tmax temperature between pl_Date and hv_Date
#
#' @examples: get_tp_rasterSummarydata(country = "Rwanda";  useCaseName = "RAB"; Crop = "Potato"; AOI = FALSE; overwrite = TRUE;
#' Planting_month_date = "07-01";  Harvest_month_date = "11-30";jobs=10, dataSource = "AgEra", scenario=TRUE)
get_tp_rasterSummarydata <- function(country, useCaseName, Crop, AOI = FALSE, overwrite = FALSE, 
                                     Planting_month_date = "02-01", Harvest_month_date = "05-30", 
                                     jobs = 10, dataSource, scenario=TRUE) {
  
  # 6.1. Initialization of input and output data ####
  
  # Input Temperature
  if(dataSource == "AgEra"){
    listRaster_AgERAmax <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMax/AgEra", pattern=".nc$", full.names = TRUE)
    listRaster_AgERAmin <- list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMin/AgEra", pattern=".nc$", full.names = TRUE)
    listYear_AgERA <- seq(1979, (1979+length(listRaster_AgERAmax))-1)
  }else{
    listRaster_AgERAmax <-list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMax/chirts", pattern=".nc$", full.names = TRUE)
    listRaster_AgERAmin <- list.files(path="/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/TemperatureMin/chirts", pattern=".nc$", full.names = TRUE)
    listYear_AgERA <- seq(1983, (1983+length(listRaster_AgERAmax))-1)
  }
  
  # Creation of the output dir
  pathOut1 <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/result/Temperature", sep="")
  
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
    foreach(i=1:length(listRaster_AgERAmax), .packages = c('terra', 'plyr', 'stringr','tidyr'), .export='.GlobalEnv') %dopar% {
      

      # Read raster
      readLayersMax <- terra::rast(listRaster_AgERAmax[i], lyrs=c(pl_Date:hv_Date))
      readLayersMin <- terra::rast(listRaster_AgERAmin[i], lyrs=c(pl_Date:hv_Date))
      
      # Crop the raster !!!!! This should be remove and done by Eduardo
      croppedLayersMax <- terra::crop(readLayersMax, countryShp)
      croppedLayersMin <- terra::crop(readLayersMin, countryShp)
      
      # Compute the total amount of rainfall
      tmax <- terra::app(croppedLayersMax, fun='mean')
      tmin <- terra::app(croppedLayersMin, fun='mean')
      
      # write ouput for the year i
      tp_summary <- c(tmax, tmin)
      names(tp_summary) <- c("tmax", "tmin")
      
      Planting_month_date2 <- gsub("-", "_", Planting_month_date)
      fname_temp <- ifelse(AOI == "TRUE", paste("Temperature_Raster_summaries_AOI_" ,Planting_month_date2, "_", dataSource,"_", listYear_AgERA[i], ".tif",sep=""), paste("Temperature_Raster_summaries_Country_",Planting_month_date2, "_", dataSource,"_", listYear_AgERA[i],".tif", sep=""))
      terra::writeRaster(tp_summary, paste(pathOut1, fname_temp, sep="/"), filetype="GTiff", overwrite=overwrite)
      
    }
    stopCluster(cls)
  }
  
  ## 6.2.2. Case planting and harvesting dates span two different year ####
  if (planting_harvest_sameYear ==  FALSE) {
    
    cls <- makeCluster(jobs)
    doParallel::registerDoParallel(cls)
    
    # Loop over all the years 
    foreach(i=1:length(listRaster_AgERAmax), .packages = c('terra', 'plyr', 'stringr','tidyr'), .export='.GlobalEnv') %dopar% {
      
      
      # Read raster
      readLayersMax1 <- terra::rast(listRaster_AgERAmax[i], lyrs=c(pl_Date:terra::nlyr(terra::rast(listRaster_AgERAmax[i]))))
      readLayersMax2 <- terra::rast(listRaster_AgERAmax[i+1], lyrs=c(1:hv_Date))
      readLayersMax <- c(readLayersMax1, readLayersMax2)
      
      readLayersMin1 <- terra::rast(listRaster_AgERAMin[i], lyrs=c(pl_Date:terra::nlyr(terra::rast(listRaster_AgERAMin[i]))))
      readLayersMin2 <- terra::rast(listRaster_AgERAMin[i+1], lyrs=c(1:hv_Date))
      readLayersMin <- c(readLayersMin1, readLayersMin2)
      
      # Crop the raster !!!!! This should be remove and done by Eduardo
      croppedLayersMax <- terra::crop(readLayersMax, countryShp)
      croppedLayersMin <- terra::crop(readLayersMin, countryShp)
      
      # Compute the total amount of rainfall
      tmax <- terra::app(croppedLayersMax, fun='mean')
      tmin <- terra::app(croppedLayersMin, fun='mean')
      
      # write ouput for the year i
      tp_summary <- c(tmax, tmin)
      names(tp_summary) <- c("tmax", "tmin")
      
      Planting_month_date2 <- gsub("-", "_", Planting_month_date)
      fname_temp <- ifelse(AOI == "TRUE", paste("Temperature_Raster_summaries_AOI_" ,Planting_month_date2, "_", dataSource,"_", listYear_AgERA[i], ".tif",sep=""), paste("Temperature_Raster_summaries_Country_" ,Planting_month_date2, "_", dataSource,"_", listYear_AgERA[i],".tif", sep=""))
      terra::writeRaster(tp_summary, paste(pathOut1, fname_temp, sep="/"), filetype="GTiff", overwrite=overwrite)
      
    }
    stopCluster(cls)
  }
  
  # 6.3. Compute the scenario (below, average, above) of each temperature parameters ####
  
  # Define the list of summary rasters to use to compute the scenario
  if (AOI == FALSE & scenario == TRUE){
    listRaster_summary <- list.files(path=pathOut1, pattern=paste("Temperature_Raster_summaries_Country_",Planting_month_date2, "_", dataSource, sep=""), full.names=T)
  }
  if (AOI == TRUE & scenario == TRUE) {
    listRaster_summary <- list.files(path=pathOut1, pattern=paste("Temperature_Raster_summaries_AOI_" ,Planting_month_date2, "_", dataSource, sep=""), full.names=T)
  }
  
  ## 6.3.1. Loop on all temperature paramaters ####
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
  fname_scenario <- ifelse(AOI == "TRUE", paste("Temperature_Raster_scenario_AOI_",Planting_month_date2, "_", dataSource, ".tif",sep=""), paste("Temperature_Raster_scenario_Country_" ,Planting_month_date2, "_", dataSource,".tif", sep=""))
  
  # Below
  terra::writeRaster(below, paste(pathOut1, paste0("Below_",fname_scenario), sep="/"), filetype="GTiff", overwrite=overwrite)
  # Average
  terra::writeRaster(average, paste(pathOut1, paste0("Average_",fname_scenario), sep="/"), filetype="GTiff", overwrite=overwrite)
  # Above
  terra::writeRaster(above, paste(pathOut1, paste0("Above_",fname_scenario), sep="/"), filetype="GTiff", overwrite=overwrite)
  
}



    