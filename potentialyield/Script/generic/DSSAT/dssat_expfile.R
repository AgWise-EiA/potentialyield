#' Create one experimental file (repetitive function)
#'
#' @param i point/folder from a list
#'
#' @return
#'
#' @examples create_filex(1)

create_filex <-function(i){
  setwd(path.to.temdata)
  
  #Read in original FileX
  file_x <- DSSAT::read_filex(filex_temp)
  #Set the experimental directory
  setwd(paste(path.to.extdata,paste0('EXTE', formatC(width = 4, as.integer((i)), flag = "0")), sep = "/"))
  #Make proposed changes to FileX
  file_x$FIELDS$WSTA<-paste0("WHTE", formatC(width = 4, as.integer((i)), flag = "0"))
  file_x$FIELDS$ID_SOIL<-paste0('TRAN', formatC(width = 5, as.integer((i)), flag = "0"))
  file_x$CULTIVARS$CR <- code
  file_x$`PLANTING DETAILS`$PDATE <- as.POSIXct(coords$plantingDate[i])
  file_x$`INITIAL CONDITIONS`$ICDAT <- as.POSIXct(coords$plantingDate[i])  #Meanwhile the same date than the planting date
  file_x$`SIMULATION CONTROLS`$SDATE <- as.POSIXct(coords$plantingDate[i])
  file_x$`HARVEST DETAILS`$HDATE <- as.POSIXct(coords$harvestDate[i])
  
  ex_profile <- suppressWarnings(DSSAT::read_sol("SOIL.SOL", id_soil = paste0('TRAN', formatC(width = 5, as.integer((i)), flag = "0"))))
  file_x$`INITIAL CONDITIONS`$SH2O<- ex_profile$SDUL #Assume field capacity as initial condition
  #Overwrite original FileX with new values
  DSSAT::write_filex(file_x,paste0('EXTE', formatC(width = 4, as.integer((i)), flag = "0"),'.',code,'X'))
  gc()
} 



#' Create multiple experimental files
#'
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param Planting_month_date is needed only for AOI and should be provided as month_date, for trial locations the actual planting date is be used so no need to change the default value
#'
#' @return
#' @export
#'
#' @examples dssat.expfile(country = "Rwanda",  useCaseName = "RAB", Crop = "Maize", AOI = FALSE, filex_temp="MZRL8142.MZX", Planting_month_date = NULL,jobs=10)


dssat.expfile <- function(country, useCaseName, Crop, AOI = FALSE,filex_temp="MZRM8143.MZX", Planting_month_date=NULL,Harvest_month_date=NULL,jobs=10, ID="TLID"){  #xmin,xmax,ymin,ymax,res,jobs,ex.name,path.to.extdata){

  # Input point data AOI / Trial
  if(AOI == TRUE){
    # countryCoord <- readRDS(paste("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_",useCaseName, "/", Crop, "/raw/AOI_GPS.RDS", sep=""))
    countryCoord <- readRDS(paste("~/agwise-datacuration/dataops/datacuration/Data/useCase_", country, "_",useCaseName, "/", Crop, "/result/AOI_GPS.RDS", sep=""))
    
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
    GPS_fieldData <- readRDS(paste("~/agwise-datacuration/dataops/datacuration/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/compiled_fieldData.RDS", sep=""))  
    countryCoord <- unique(GPS_fieldData[, c("lon", "lat", "plantingDate", "harvestDate", ID)])
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    names(countryCoord) <- c("longitude", "latitude", "plantingDate", "harvestDate", "ID")
    ground <- countryCoord
  }
  
  
  
  ground$Planting <- as.Date(ground$plantingDate, "%Y-%m-%d") # Planting date in Date format
  ground$Harvesting <- as.Date(ground$harvestDate, "%Y-%m-%d") # Harvesting date in Date format
  
  pathIn <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/raw/profile/", sep="")
  
  
  if(AOI == TRUE){
    Rainfall <- readRDS(paste(pathIn, "Rainfall_PointData_AOI.RDS", sep=""))
    Soil <- readRDS(paste(pathIn, "SoilDEM_PointData_AOI.RDS", sep=""))

  }else{
    Rainfall <- readRDS(paste(pathIn, "Rainfall_PointData_trial.RDS", sep=""))
    Soil <- readRDS(paste(pathIn, "SoilDEM_PointData_trial.RDS", sep=""))
  }
  
  metaDataWeather <- as.data.frame(t(Rainfall[1:5, ]))
  metaDataWeather$RowNames <- rownames(metaDataWeather)
  head(metaDataWeather)
  metaData_Soil <-Soil[,c("longitude", "latitude","ID","NAME_1","NAME_2")]
  
  metaData <- merge(metaDataWeather,metaData_Soil)
  rownames(metaData) <- metaData$RowNames
  organized <-colnames(Rainfall)[!(colnames(Rainfall) %in% c("MetaDVar","Date","Month","Year"))]
  sortingIndices <- match(organized, rownames(metaData))
  # Sort metaData based on sorting indices
  metaData <- metaData[sortingIndices, ]
  coords <- merge(metaData,ground)
  coords <- coords[sortingIndices, ]
  grid <- as.matrix(coords)
  #Set working directory to save the results
  path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/result/DSSAT", sep="")
  
  #Define working directory with template data
  path.to.temdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/Landing/DSSAT", sep="")
  #We need to add more codes
  crops <- c("Maize","Potato")
  cropcode <- c("MZ","PT")
  
  cropid <- which(crops == Crop)
  code <- cropcode[cropid]
  
  #require(doParallel)
  #require(foreach)
  # Set number of parallel workers
  #cls <- parallel::makePSOCKcluster(jobs)
  #doParallel::registerDoParallel(cls)
  #Set working directory (where the file is)
  setwd(path.to.extdata)

  # Process Experimental Files
  #foreach::foreach(i=seq_along(matching_folders), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "DSSAT")) %dopar% {
  
  results <- map(seq_along(grid[,1]), create_filex) %||% print("Progress:")

}