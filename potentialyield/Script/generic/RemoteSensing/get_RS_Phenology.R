# Get Phenology derived from Remote Sensing  

# Introduction: 
# Crop phenology extraction through time-series satellite images (MODIS EVI 250m 8-day interval;Aqua & Terra data OR Sentinel-3 NDVI 10-day 300) ################

#i. Shaping of the data 
#ii. Subsetting the analysis year 
#iii.Extracting the area of interest 
#iv. Applying smoothing techniques to fill data gaps 
#v.  Saving the raster 

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


Phenology_rasterTS<-function(country, useCaseName, Planting_year, Harvesting_year, Planting_month, Harvesting_month, overwrite){
  
  #' @description Function that will TO DO
  #' @param country country name
  #' @param useCaseName use case name  name
  #' @param overwrite default is FALSE 
  #' @param Planting_year the planting year in integer
  #' @param Harvesting_year the harvesting year in integer
  #' @param Planting_month the planting month in integer
  #' @param Harvesting_month the harvesting month in integer
  #'
  #' @return raster files cropped from global data and the result will be written out in agwise-datasourcing/dataops/datasourcing/Data/useCaseName/MODISdata/transform/EVI
  #'
  #' @examples smooth_rasterTS(country = "Rwanda", useCaseName = "RAB", Planting_year = 2021, Harvesting_year = 2021, overwrite = TRUE)
  #' 
  #' 
  #' 
  ## 2.1. Creating a directory to store the phenology data ####
  pathOut <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", "RSdata/transform/EVI", sep="")
  
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }
  
  ## 2.2. Read  and prepare the relevant data ####
  
  ## Download the extent of the country of interest
  countryShp <- geodata::gadm(country, level = 2, path='.')
  
  ## Read the preprocessed RS time series
  pathIn <- paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_",useCaseName, "/", "MODISdata/transform/EVI", sep="")
  fileIn_name <- paste0(country,'_', useCaseName, '_*_EVI_', Planting_year,'_', Harvesting_year, '_SG.tif')
  listRaster_SG <- list.files(path=pathIn, pattern=glob2rx(fileIn_name), full.names = T)
  stacked_SG <- terra::rast(listRaster_SG) #stack

  ## 2.3. Extraction of date with Peak/Max VI values ####
  ##  Choose those images in cropping season between Planting_Month and Harvesting_Month which can represent the date range when the VI is maximum
  ##  This will create a raster having values of Julian days for every pixel where VI is maximum
 
  ## Subset the cropping season +/- 15 days
  # Start of the season
  start <- paste0("01-",Planting_month,"-", Planting_year)
  start <- as.Date(as.character(start), format ="%d-%B-%Y")
  startj <- as.POSIXlt(start)$yday # conversion in julian day
  
  # End of the season
  end <- paste0("31-",Harvesting_month,"-", Planting_year)
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
  
  ## Pixels having maximum VI values
  peakmx.max <- terra::app(stacked_SG_s, fun=max)
  # plot(peakmx.max, main ="Pixels having maximum values")
  
  ## Create an empty raster to include the Julian days info against its respective calendar date when the pixels have maximum values in the cropping season 
  max.pheno <- peakmx.max
  terra::values(max.pheno) <- NA
  
  ## To make map of Julian day of "max.pheno" layer; when crop reaches its peak VI
  # keeping range of those calendar dates in the cropping season where crop EVI values can be maximum
  
  initial_names <-names(stacked_SG_s)
  calendar_dates <- as.integer(substr(initial_names, nchar(initial_names) - 3 + 1, nchar(initial_names))) # extract the last 3 characters corresponding to "ddd"
  max_pheno_julian <- setNames(calendar_dates, initial_names)
  
  # Loop to assign the julian date of peak VI values
  for (i in initial_names) {
    max.pheno[stacked_SG_s[[i]] == peakmx.max] <- max_pheno_julian[i]
  }

  ## 2.4. Extraction of date with Min VI values ####
  # calculate minimum using the same stack as above
  
  ## Pixels having minimum VI values
  
  low.min <- terra::app(stacked_SG_s, fun=min)
  <- calc(mx.stk, fun=min)
  plot(low.min)
  ## create an empty raster that matches the extent of the input satellite images, 
  # It will be used to include the Julian days info against its respective calendar date when the pixels have minimum values in the ascending limb of the growth curve in a crop season
  min.pheno <-raster(ncol=979, nrow=850, xmn=28.85444, xmx=30.89326, ymn=-2.826696, ymx=-1.054445)
  projection(min.pheno)="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #assign projection to match
  
  # make map of Julian day of min pheno; when crop reaches its peak EVI
  ## keeping range of those calendar dates in the cropping season where crop EVI values can be minimum
  
  # List of calendar dates
  calendar_date <- c("Sep06_21", "Sep14_21", "Sep22_21", "Sep30_21", "Oct08_21", "Oct16_21","Oct24_21", "Nov01_21")
  julian_day <- c(249, 257, 265, 273, 281, 289, 297, 305)
  min_pheno_julian <- setNames(julian_day, calendar_date)
  
  
  ## LOGIC: If pixels from Sep06 images matches with Min value raster (low.min)then assign Sep 06 as that Julian day ####
  #loop
  for (date in calendar_date) {
    min.pheno[Mod_sgflt_2021_22b[[date]] == low.min] <- min_pheno_julian[date]
  }
  
  par(mfrow=c(1,1))
  hist(min.pheno) ## check which Julian day has minimum pixels
  col.pd<- brewer.pal(5, "Spectral")
  plot(min.pheno, col= col.pd,main="Julian days when EVI reaches its lowest in the ascending limb of the growth curve of the cropping season")
  
  
  ## Amplitude calculation
  ################################################
  ## Lobell et al (2013): http://dx.doi.org/10.1016/j.agsy.2012.09.003 #mentioned green-up in this study is defined as the point when a fitted curve reaches 10% of that year’s maximum amplitude.
  
  ### Calculate Amplitude
  amplit <- (peakmx.max+low.min)/2
  ## Max minus Amplitude
  max_amplit <- peakmx.max-amplit
  # 10 percent of max amplitude 
  max_amplit10pc <- max_amplit*0.1
  ## add minimum to it to generate a range between minimum and 10% of amplitude in which the green up dates will fall
  max_min_amplit10pc <- low.min+max_amplit10pc
  hist(max_min_amplit10pc)
  plot(max_min_amplit10pc)
  ##create an empty raster that matches the extent of the other rasters, this will have all the reclassified values
  pd.pct10max <-raster(ncol=979, nrow=850, xmn=28.85444, xmx=30.89326, ymn=-2.826696, ymx=-1.054445)
  projection(pd.pct10max)="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #assign projection to match
  
  ## conditions to find Julian days from amplitude and minimum EVI data
  #when a fitted curve reaches 10% of that year’s maximum amplitude
  ## Kept reasonable "green up Julian date" range where most of the planting happens in ascending limb of the growth curve
  
  julian_days <- c(249, 257, 265, 273, 281, 289, 297, 305)
  date_columns <- c("Sep06_21", "Sep14_21", "Sep22_21", "Sep30_21", "Oct08_21", "Oct16_21", "Oct24_21", "Nov01_21")
  julian_da <- setNames(julian_days, date_columns)
  
  
  ## LOGIC: If pixels from Sep06 image are in range between minimum and 10% of amplitude as that Julian day then assign the pixel that Julian day (249) ####
  ## loop
  for (date in date_columns) {
    pd.pct10max[Mod_sgflt_2021_22b[[date]] <= max_min_amplit10pc & Mod_sgflt_2021_22b[[date]] > low.min] <- julian_da[date]
  }
  
  
  par(mfrow=c(1,1))
  hist(pd.pct10max)  ## check the distribution Julian day that has 10%
  #col.pd<- brewer.pal(8, "Spectral")
  plot(rwd) # plot boundary
  plot(pd.pct10max, col= col.pd,main="Julian days when GREEN UP is visible", add=T)
 
}

country = "Rwanda"
useCaseName = "RAB"
Planting_year = 2021
Harvesting_year = 2021
Planting_month = "February"
Harvesting_month = "July"
overwrite = TRUE
