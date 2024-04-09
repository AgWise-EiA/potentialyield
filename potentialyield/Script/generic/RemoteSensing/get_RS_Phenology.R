# Get Phenology (Planting and harvesting dates) derived from MODIS NDVI time series for the Use Case  

# Introduction: 
# This script allows the crop phenology extraction through MODIS NDVI time series. This script has to be run after get_MODISts_PreProc.R. It covers :
# (1) - Shaping of the data 
# (2) - Extracting the peak/max values and date 
# (3) - Extracting the date of the min values on the left part of the curve  
# (4) - Extracting the date of the min values on the right part of the curve
# (5) - Extracting the actual planting date
# (6) - Saving the rasters for actual planting date and harvesting date

#### Getting started #######

# 1. Sourcing required packages -------------------------------------------
packages_required <- c("plotly", "raster", "rgdal", "gridExtra", "sp", "ggplot2", "caret", "signal", "timeSeries", "zoo", "pracma", "rasterVis", "RColorBrewer", "dplyr", "terra", "geodata" )

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))

# 2. Extracting phenology from NDVI time series  -------------------------------------------


Phenology_rasterTS<-function(country, useCaseName, Planting_year, Harvesting_year, Planting_month, Harvesting_month, overwrite = FALSE){
  
  #' @description Function that will allow to obtain the actual planting date and the harvesting date based on VI time series analysis
  #' @param country country name
  #' @param useCaseName use case name  name
  #' @param overwrite default is FALSE 
  #' @param Planting_year the planting year in integer
  #' @param Harvesting_year the harvesting year in integer
  #' @param Planting_month the planting month in full name (eg.February)
  #' @param Harvesting_month the harvesting month in full name (eg. September)
  #'
  #' @return raster files of planting date and harvesting date at the Use Case level, the results will be written out in /agwise-potentialyield/dataops/potentialyield/Data/useCase/RSdata/transform/EVI
  #'
  #' @examples Phenology_rasterTS(country = "Rwanda", useCaseName = "RAB",Planting_year = 2021, Harvesting_year = 2021, Planting_month = "February",Harvesting_month = "July", overwrite = TRUE)

  #' 
  #' 
  ## 2.1. Creating a directory to store the phenology data ####
  pathOut <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", "RSdata/transform/NDVI", sep="")
  
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }
  
  ## 2.2. Read  and prepare the relevant data ####
  
  ## Download the extent of the country of interest
  #countryShp <- geodata::gadm(country, level = 2, path='.')
  
  ## Read the preprocessed RS time series
  pathIn <- paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_",useCaseName, "/", "MODISdata/transform/NDVI", sep="")
  fileIn_name <- paste0(country,'_', useCaseName, '_*_NDVI_', Planting_year,'_', Harvesting_year, '_SG.tif')
  listRaster_SG <- list.files(path=pathIn, pattern=glob2rx(fileIn_name), full.names = T)
  stacked_SG <- terra::rast(listRaster_SG) #stack

 
  ## 2.3. Subset the cropping season +/- 15 days ####
  # Start of the season
  start <- paste0("01-",Planting_month,"-", Planting_year)
  start <- as.Date(as.character(start), format ="%d-%B-%Y")
  startj <- as.POSIXlt(start)$yday # conversion in julian day
  
  # Test the number of days in a month
  if (Harvesting_month %in% c('January','March','May','July','August','October','December')){
    nday = "31-"
  }
  
  if (Harvesting_month %in% c('April','June','September','November')){
    nday = "30-"
  }
  if (Harvesting_month %in% c('February')){
    nday = "28-"
  }
  
  # End of the season
  end <- paste0(nday,Harvesting_month,"-", Planting_year)
  end <- as.Date(as.character(end), format ="%d-%B-%Y")
  endj <- as.POSIXlt(end)$yday # conversion in julian day
  
  # Create a sequence between start and end of the season +/- 15 days
  # Case Planting Year = Harvesting Year
  if (Planting_year == Harvesting_year){
    seq<- seq(startj-15, endj+15,by=1)
    seq= paste0(Planting_year,"_", formatC(seq, width=3, flag="0"))
  }
  
  # Case Planting Year < Harvesting Year
  if (Planting_year < Harvesting_year){
    seq1<- seq(startj-15, 365,by=1)
    seq1 <- paste0(Planting_year, "_", formatC(seq1, width=3, flag="0"))
    seq2 <- seq(1, endj+15,by=1)
    seq2 <- paste0(Harvesting_year, "_", formatC(seq2, width=3, flag="0"))
    seq= c(seq1, seq2)
  }
  
  # Case Planting Year > Harvesting Year
  if (Planting_year > Harvesting_year){
    stop( "Planting_year can't be > to Harvesting_year")
  }
  
  # Subset the data between planting and harvesting date
  stacked_SG_s <- stacked_SG[[grep(paste(seq, collapse = "|"), names(stacked_SG))]]
  rm(stacked_SG)
  
  ## 2.4. Extraction of date with Peak/Max VI values ####
  
  # Defining the names of the layer in julian day
  initial_names <-names(stacked_SG_s)
  calendar_dates <- as.integer(substr(initial_names, nchar(initial_names) - 3 + 1, nchar(initial_names))) # extract the last 3 characters corresponding to "ddd"
  max_pheno_julian <- setNames(calendar_dates, initial_names)
  
  ##  Choose those images in cropping season between Planting_Month and Harvesting_Month which can represent the date range when the VI is maximum
  ##  This will create a raster having values of Julian days for every pixel where VI is maximum
  
  ## Pixels having maximum VI values
  peakmx.max <- terra::app(stacked_SG_s, fun=max)
  # plot(peakmx.max, main ="Pixels having maximum values")
  
  # CHECK HOW WROTE THIS #
  #max.pheno <- terra::which.max(stacked_SG_s)
  #max.pheno <- classify(max.pheno, cbind(1:nlyr(stacked_SG_s, max_pheno_julian)))  

  ## Create an empty raster to include the Julian days info against its respective calendar date when the pixels have maximum values in the cropping season 
  max.pheno <- peakmx.max
  terra::values(max.pheno) <- NA
  
  ## To make map of Julian day of "max.pheno" layer; when crop reaches its peak VI
  # keeping range of those calendar dates in the cropping season where crop EVI values can be maximum
  # Loop to assign the julian date of peak VI values
  for (i in initial_names) {
    max.pheno[stacked_SG_s[[i]] == peakmx.max] <- max_pheno_julian[i]
  }

  ## Median peak values over the area
  median.max <- median(terra::values(max.pheno), na.rm = TRUE)
                        
  ## 2.5. Extraction of date with Min VI values left ####
  # We are looking at the TS comprise between the start of the season and the mean peak date values over the area
  # Subset the data between planting and median peak of the season date
  
  # Case Planting Year = Harvesting Year
  if (Planting_year == Harvesting_year){
    seq.left <- seq(startj-15, median.max,by=1)
    seq.left <- paste0(Planting_year,"_", formatC(seq.left, width=3, flag="0"))
  }

  # Case Planting Year < Harvesting Year
  if (Planting_year < Harvesting_year){
    # Case median date is on same year that planting year
    if (median.max <= 365 & median.max > startj){
      seq.left <- seq(startj-15, median.max,by=1)
      seq.left <- paste0(Planting_year,"_", formatC(seq.left, width=3, flag="0"))
    }
    
    # Case median date is on same year that harvesting year
    if (median.max >= 1 & median.max < endj){
      seq.left1 <- seq(startj-15, 365,by=1)
      seq.left1 <- paste0(Planting_year,"_", formatC(seq.left1, width=3, flag="0"))
      seq.left2 <- seq(1, median.max,by=1)
      seq.left2 <- paste0(Harvesting_year,"_", formatC(seq.left2, width=3, flag="0"))
      seq.left <- c(seq.left1, seq.left2)
    }
  }
 
  stacked_SG_left <- stacked_SG_s[[grep(paste(seq.left, collapse = "|"), names(stacked_SG_s))]]
  
  # Defining the names of the layer in julian day
  initial_names_left <-names(stacked_SG_left)
  calendar_dates_left <- as.integer(substr(initial_names_left, nchar(initial_names_left) - 3 + 1, nchar(initial_names_left))) # extract the last 3 characters corresponding to "ddd"
  min_pheno_julian_left <- setNames(calendar_dates_left, initial_names_left)
  
  ## Pixels having minimum VI values in the left part of the curve
  low.min.left <- terra::app(stacked_SG_left, fun=min, na.rm=TRUE)
  
  ## Create an empty raster to include the Julian days info against its respective calendar date when the pixels have maximum values in the cropping season 
  min.pheno.left <- low.min.left
  terra::values(min.pheno.left) <- NA
  
  ## To make map of Julian day of "min.pheno" layer; when crop reaches its min VI
  # keeping range of those calendar dates in the cropping season where crop VI values can be min
  # Loop to assign the julian date of min VI values
  for (i in initial_names_left) {
    min.pheno.left[stacked_SG_left[[i]] == low.min.left] <- min_pheno_julian_left[i]
  }
  
  ## 2.6. Extraction of date with Min VI values right ####
  # We are looking at the TS comprise between the median peak of the season and the end of the season
  # Subset the data between median peak of the season date and end of the season
  
  # Case Planting Year = Harvesting Year
  if (Planting_year == Harvesting_year){
    seq.right <- seq(median.max,endj+15, by=1)
    seq.right <- paste0(Planting_year,"_", formatC(seq.right, width=3, flag="0"))
  }
  
  # Case Planting Year < Harvesting Year
  if (Planting_year < Harvesting_year){
    # Case median date is on same year that planting year
    if (median.max <= 365 & median.max > startj){
      seq.right1 <- seq(median.max, 365, by=1)
      seq.right1 <- paste0(Planting_year,"_", formatC(seq.right1, width=3, flag="0"))
      seq.right2 <- seq(1, endj+15, by=1)
      seq.right2 <- paste0(Harvesting_year,"_", formatC(seq.right2, width=3, flag="0"))
      seq.right <- c(seq.right1, seq.right2)
    }
    
    # Case median date is on same year that harvesting year
    if (median.max >= 1 & median.max < endj){
      seq.right <- seq(median.max, endj+15,by=1)
      seq.right <- paste0(Harvesting_year,"_", formatC(seq.right, width=3, flag="0"))
    }
  }
  
  stacked_SG_right <- stacked_SG_s[[grep(paste(seq.right, collapse = "|"), names(stacked_SG_s))]]
  
  # Defining the names of the layer in julian day
  initial_names_right <-names(stacked_SG_right)
  calendar_dates_right <- as.integer(substr(initial_names_right, nchar(initial_names_right) - 3 + 1, nchar(initial_names_right))) # extract the last 3 characters corresponding to "ddd"
  min_pheno_julian_right <- setNames(calendar_dates_right, initial_names_right)
  
  ## Pixels having minimum VI values in the left part of the curve
  low.min.right <- terra::app(stacked_SG_right, fun=min, na.rm=TRUE)
  
  ## Create an empty raster to include the Julian days info against its respective calendar date when the pixels have maximum values in the cropping season 
  min.pheno.right <- low.min.right
  terra::values(min.pheno.right) <- NA
  
  ## To make map of Julian day of "min.pheno" layer; when crop reaches its min VI
  # keeping range of those calendar dates in the cropping season where crop VI values can be min
  # Loop to assign the julian date of min VI values
  for (i in initial_names_right) {
    min.pheno.right[stacked_SG_right[[i]] == low.min.right] <- min_pheno_julian_right[i]
  }
  
  ## 2.7. Amplitude calculation between base level and max/peak ####
  ## TIMESAT : https://web.nateko.lu.se/timesat/docs/TIMESAT33_SoftwareManual.pdf # amplitude is computed as the difference between the base level (mean on min left and min right) and peak value of the season.
  ## Lobell et al (2013): http://dx.doi.org/10.1016/j.agsy.2012.09.003 #mentioned green-up in this study is defined as the point when a fitted curve reaches 10% of that year’s maximum amplitude.
  
  ## Calculate the base level
  basel <- c(low.min.left, low.min.right)
  basel <- terra::app(basel, fun='mean')
  ## Calculate Amplitude !!!! Need to be checked with timesat
  amplit <- (peakmx.max-basel)
  ## Max minus Amplitude
  #max_amplit <- peakmx.max-amplit
  ## 10 percent of max amplitude 
  amplit10pc <- amplit*0.1
  
  ### 2.7.1. Left side : find the date for 10% values : Green up ####
  
  ## left side : add minimum to it to generate a range between minimum left and 10% of amplitude in which the green up dates will fall
  min_amplit10pc_left <- low.min.left + amplit10pc
  
  ##create an empty raster that matches the extent of the other rasters, this will have all the reclassified values
  pd.pct10max.left <-min_amplit10pc_left
  terra::values(pd.pct10max.left) <- NA
  
  ## conditions to find Julian days from amplitude and minimum VI data
  # when a fitted curve reaches 10% of that year’s maximum amplitude
  ## Kept reasonable "green up Julian date" range where most of the planting happens in ascending limb of the growth curve
  ## LOGIC: If pixels from Sep06 image are in range between minimum and 10% of amplitude as that Julian day then assign the pixel that Julian day (249) ####
  ## loop
  for (i in initial_names_left) {
    pd.pct10max.left[stacked_SG_left[[i]] <= min_amplit10pc_left & stacked_SG_left[[i]] > low.min.left] <- min_pheno_julian_left[i]
  }
  
  ### 2.7.2. Right side : find the date for 10% values : Scenescence ####
  ## right side : add minimum to it to generate a range between minimum right and 10% of amplitude in which the senescence dates will fall
  min_amplit10pc_right <- low.min.right + amplit10pc
  
  ##create an empty raster that matches the extent of the other rasters, this will have all the reclassified values
  pd.pct10max.right <-min_amplit10pc_right
  terra::values(pd.pct10max.right) <- NA
  
  ## conditions to find Julian days from amplitude and minimum VI data
  # when a fitted curve reaches 10% of that year’s maximum amplitude
  ## Kept reasonable "green up Julian date" range where most of the planting happens in ascending limb of the growth curve
  ## LOGIC: If pixels from Sep06 image are in range between minimum and 10% of amplitude as that Julian day then assign the pixel that Julian day (249) ####
  ## loop
  for (i in initial_names_right) {
    pd.pct10max.right[stacked_SG_right[[i]] <= min_amplit10pc_right & stacked_SG_right[[i]] > low.min.right] <- min_pheno_julian_right[i]
  }
  
  ## 2.8. Actual planting dates ####
  ## Singh et al (2019): https://doi.org/10.1038/s41893-019-0304-4 pixel achieved 10% of maximum VI on the ascending limb of the growth curve; actual ##transplanting is likely to be 2–3 weeks (~21days) before the green up estimates.
  
  ## Remove 21 Days from the emergence date
  pd.pct10max.left21D <- pd.pct10max.left-21 # 21 days need to be done differently

  ## Saving the raster of actual planting date
  filename <- paste0(country,'_', useCaseName, '_MODIS_NDVI_', Planting_year,'_',Planting_month,'_', Harvesting_year,'_', Harvesting_month, '_ActualPlantingDate.tif')
  terra::writeRaster(pd.pct10max.left21D, paste(pathOut, filename, sep="/"), filetype="GTiff", overwrite=overwrite)
  
  ## 2.9. Actual Harvesting dates ####
 
  ## Saving the raster of actual planting date
  filename <- paste0(country,'_', useCaseName, '_MODIS_NDVI_', Planting_year,'_',Planting_month,'_', Harvesting_year,'_', Harvesting_month, '_ActualHarvestingDate.tif')
  terra::writeRaster(pd.pct10max.right, paste(pathOut, filename, sep="/"), filetype="GTiff", overwrite=overwrite)
  
}

 # country = "Kenya"
 # useCaseName = "KALRO"
 # Planting_year = 2017
 # Harvesting_year = 2017
 # Planting_month = "February"
 # Harvesting_month = "November"
 # overwrite = TRUE
