
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
## function for crop models geostoatial data 
#################################################################################################################
# Extract geo spatial data time series for point based data prepared for Crop Models
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
#' @param dataSource is one of c("CHIRPS", "AgEra")
#' @param ID only when AOI  = FALSE, it is the column name Identifying the trial ID in compiled_fieldData.RDS
#' 
#' @return a data frame with file name made to reflect point/summary, AOI/trial,  planting as mm_dd and source CHIRPS/AgEra
#' the data frame contains daily rainfall for every GPS point. 
#' For the trial sites: it provides:longitude, latitude, plantingDate and harvestDate as yyyy-mm-dd, yearPi & yearHi as yyyy, 
#' pl_j & hv_j as date of the year for panting & harvest, growinglength and daily rainfall by date, NA for dates not part of the growing season for a trial
#' For AOI: it provides dily rainfall for ll dates between Planting_month_date and Harvest_month_dates plus 
#' plantingYear, harvestYear, longitude, latitude   
#' 
#' @examples: get_summaries(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE, overwrite = TRUE,
#'              Planting_month_date = "07-01",  Harvest_month_date = "11-30", jobs=10, id = "TLID")
get_data_4CropModels <- function(country, useCaseName, Crop, AOI = FALSE, overwrite = FALSE, 
                               Planting_month_date = "02-01", Harvest_month_date = "05-30", 
                               jobs = 10, dataSource, ID, varName = NULL){
  
  # 4.1. Initialization of input and output data ####
  
  # Input rainfall
  if(dataSource == "CHIRPS"){
    listRaster_RF <-list.files(path=paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/", varName,"/chirps",sep=""), pattern=".nc$", full.names = TRUE)
  }else{
    listRaster_RF <-list.files(path=paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/",varName,"/AgEra", sep=""), pattern=".nc$", full.names = TRUE)
  }
  
  # Creation of the output dir
  pathOut1 <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/result/geo_4cropModel/",varName, sep="")
  pathOut2 <- paste("/home/jovyan/agwise/AgWise_Data/potential_yield/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/geo_4cropModel/",varName, sep="")
  
  if (!dir.exists(pathOut1)){
    dir.create(file.path(pathOut1), recursive = TRUE)
  }
  
  if (!dir.exists(pathOut2)){
    dir.create(file.path(pathOut2), recursive = TRUE)
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
      rf_4CP <- foreach(i=1:length(listRaster_RF), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        rasti <- listRaster_RF[i]
        PlHvD <- terra::rast(rasti, lyrs=c(pl_j:hv_j))
        xy <- ground[, c("longitude", "latitude")]
        raini <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
        raini <- raini[,-1]
        
        if( varName %in% c("TemperatureMin", "TemperatureMax")){
          raini <- raini-273
        }
        
        ## name the columns by date in the form of dd_mm_yyyy
        rainyDssat <- raini
        dates_nr <- sub("^[^_]+", "", names(rainyDssat))
        dates_nr <- as.numeric(gsub("_", "", dates_nr))
        year_nr <- str_extract(rasti, "[[:digit:]]+")
        startday <- paste(strsplit(Planting_month_date, "-")[[1]][2], strsplit(Planting_month_date, "-")[[1]][1], year_nr, sep="_")
        
        cnames <- startday
        for(h in 1:length(dates_nr)){
          dd <- as.character(as.Date(h, origin = paste(year_nr, strsplit(Planting_month_date, "-")[[1]][1], strsplit(Planting_month_date, "-")[[1]][2], sep="-")))
          cnames <- c(cnames, paste(strsplit(dd, "-")[[1]][3], strsplit(dd, "-")[[1]][2], strsplit(dd, "-")[[1]][1], sep="_"))
        }
        names(rainyDssat) <- cnames[-length(cnames)]
        
        rainyDssat_t <- as.data.frame(t(rainyDssat))
        colnames(rainyDssat_t) <- paste("Point", c(1:ncol(rainyDssat_t)), sep="_")
        rainyDssat_t$MetaDVar <- rownames(rainyDssat_t)
        return(rainyDssat_t)
        # CP <- cbind(ground, rainyDssat_t)
      }
      
      rainfall_points <- do.call(rbind, rf_4CP)
      stopCluster(cls)
      
      metadata_D <- as.data.frame(t(data.frame(longitude = ground$longitude, latitude = ground$latitude,
                                               NAME_1 = ground$NAME_1, NAME_2 = ground$NAME_2)))
      metadata_D$MetaDVar <- rownames(metadata_D)
      colnames(metadata_D) <- colnames(rainfall_points)
      rainfall_points <- rbind(metadata_D, rainfall_points)
    }
    
    ## 4.2.2. Case planting and harvesting dates span two different years ####
    if (planting_harvest_sameYear ==  FALSE) {
      
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      ## RelativeHumidity
      rf_4CP <- foreach(i = 1:(length(listRaster_RF)-1), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        listRaster_RF <- listRaster_RF[order(listRaster_RF)]
        rast1 <- listRaster_RF[i]
        rast2 <- listRaster_RF[i+1]
        rasti1 <- terra::rast(rast1, lyrs=c(pl_j:terra::nlyr(terra::rast(rast1))))
        rasti2 <- terra::rast(rast2, lyrs=c(1:hv_j))
        PlHvD <- c(rasti1, rasti2)
        xy <- ground[, c("longitude", "latitude")]
        raini <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
        raini <- raini[,-1]
        
        if( varName %in% c("TemperatureMin", "TemperatureMax")){
          raini <- raini-273
        }
        
        ## name the columns by date in the form of dd_mm_yyyy
        rainyDssat <- raini
        dates_nr <- sub("^[^_]+", "", names(rainyDssat))
        dates_nr <- as.numeric(gsub("_", "", dates_nr))
        year_nr <- str_extract(rasti, "[[:digit:]]+")
        startday <- paste(strsplit(Planting_month_date, "-")[[1]][2], strsplit(Planting_month_date, "-")[[1]][1], year_nr, sep="_")
        
        cnames <- startday
        for(h in 1:length(dates_nr)){
          dd <- as.character(as.Date(h, origin = paste(year_nr, strsplit(Planting_month_date, "-")[[1]][1], strsplit(Planting_month_date, "-")[[1]][2], sep="-")))
          cnames <- c(cnames, paste(strsplit(dd, "-")[[1]][3], strsplit(dd, "-")[[1]][2], strsplit(dd, "-")[[1]][1], sep="_"))
        }
        names(rainyDssat) <- cnames[-length(cnames)]
        
        rainyDssat_t <- as.data.frame(t(rainyDssat))
        colnames(rainyDssat_t) <- paste("Point", c(1:ncol(rainyDssat_t)), sep="_")
        rainyDssat_t$MetaDVar = rownames(rainyDssat_t)
        return(rainyDssat_t)
      }
      rainfall_points <- do.call(rbind, rf_4CP)
      stopCluster(cls)
      
      metadata_D <- as.data.frame(t(data.frame(longitude = ground$longitude, latitude = ground$latitude,
                                               NAME_1 = ground$NAME_1, NAME_2 = ground$NAME_2)))
      metadata_D$MetaDVar <- rownames(metadata_D)
      colnames(metadata_D) <- colnames(rainfall_points)
      rainfall_points <- rbind(metadata_D, rainfall_points)
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
    
   
    ##Loop on all the trial location to calculate the seasonal rainfall parameters ####
    
    rainfall_points <- NULL
    for(i in 1:nrow(ground)){
      print(i)
      
      # Extract the i-th row
      groundi <- ground[i, c("longitude", "latitude", "plantingDate", "harvestDate",ID,"NAME_1", "NAME_2","yearPi", "yearHi", "pl_j", "hv_j")]
      
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
      
      if( varName %in% c("TemperatureMin", "TemperatureMax")){
        raini <- raini-273
      }
      
      ##
      
      ## name the columns by date in the form of dd_mm_yyyy
      rainyDssat <- raini
      dates_nr <- sub("^[^_]+", "", names(rainyDssat))
      dates_nr <- as.numeric(gsub("_", "", dates_nr))
      
      
      plantingDate <- as.character(groundi$plantingDate)
      startday <- paste(strsplit(plantingDate, "-")[[1]][3], strsplit(plantingDate, "-")[[1]][2], yearPi, sep="_")
      cnames <- startday
      for(h in 1:length(dates_nr)){
        dd <- as.character(as.Date(h, origin = groundi$plantingDate))
        cnames <- c(cnames, paste(strsplit(dd, "-")[[1]][3], strsplit(dd, "-")[[1]][2], strsplit(dd, "-")[[1]][1], sep="_"))
      }
      names(rainyDssat) <- cnames[-length(cnames)]
      
      rainyDssat_t <- as.data.frame(t(rainyDssat))
      colnames(rainyDssat_t) <- paste("Point", i, sep="_")
      rainyDssat_t$MetaDVar <- rownames(rainyDssat_t)
      
      
      if(i == 1){
        rainfall_points <- rainyDssat_t
      }else{
        rainfall_points <- merge(rainfall_points, rainyDssat_t, by="MetaDVar", all=TRUE)
      }
    }
    
    metadata_D <- as.data.frame(t(data.frame(longitude = ground$longitude, latitude = ground$latitude,
                                             NAME_1 = ground$NAME_1, NAME_2 = ground$NAME_2, ID = ground[[ID]])))
    metadata_D$MetaDVar <- rownames(metadata_D)
    colnames(metadata_D) <- colnames(rainfall_points)
    rainfall_points <- rbind(metadata_D, rainfall_points)
  }
  
  
  x <- gsub("_", "-",rainfall_points$MetaDVar[6:nrow(rainfall_points)])
  y <- as.Date(x, format="%d-%m-%Y")
  
  Date <- day(y)
  Month <- month(y)
  Year <- year(y)
  
  Date <- ifelse(Date < 10, paste(0,Date,sep=""), as.character(Date))
  Month <- ifelse(Month < 10, paste(0,Month,sep=""), as.character(Month))
  
  
  rainfall_points$Date <- c(rep(NA, 5), Date)
  rainfall_points$Month <- c(rep(NA, 5), Month)
  rainfall_points$Year <- c(rep(NA, 5), Year)
  
  # 4.4. writing the output ####
  
  # Check if the directory exists
  Planting_month_date <- gsub("-", "_", Planting_month_date)
  fname_rain <- ifelse(AOI == "TRUE", paste(varName, "_4CM_AOI_", Planting_month_date, "_", dataSource, ".RDS",sep=""), paste(varName,"_4CM_trial_", dataSource, ".RDS", sep=""))
  
  saveRDS(object = rainfall_points, file=paste(pathOut1, fname_rain, sep="/"))
  saveRDS(object = rainfall_points, file=paste(pathOut2, fname_rain, sep="/"))
  
  return(rainfall_points)
}



