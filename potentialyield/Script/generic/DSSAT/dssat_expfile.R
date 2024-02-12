#################################################################################################################
## sourcing required packages 
#################################################################################################################
packages_required <- c("tidyverse", "lubridate","DSSAT")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

#detach("package:DSSAT", unload = TRUE)
devtools::install_github("palderman/DSSAT", ref = "develop",force=T,upgrade = 'always')

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))

#' Create one experimental file (repetitive function)
#'
#' @param i point/folder from a list
#' @param path.to.temdata directory with template weather and soil data in DSSAT format
#' @param filex_temp Template experimental file name in DSSAT format (FILEX)
#' @param path.to.extdata working directory to save the weather and soil data in DSSAT format
#' @param coords dataframe with the locations and metadata
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param code crop code in DSSAT format
#' @param plantingWindow number of weeks that define the planting window  considering the Planting_month_date as earliest planting week. It is given when several planting dates are to be tested to determine optimal planting date (applies to AOI)  
#' @param number_years number of years the simulations are run for the AOI (it does not apply to the trial location data)
#' 
#' @return
#'
#' @examples create_filex(1)

create_filex <-function(i,path.to.temdata,filex_temp,path.to.extdata,coords, AOI, code,plantingWindow,number_years,ingenoid, Province){
  setwd(path.to.temdata)
  
  #Read in original FileX
  file_x <- DSSAT::read_filex(filex_temp)
  #Set the experimental directory
  if(AOI==TRUE){
    #setwd(paste(path.to.extdata,"AOI",paste0('EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/"))
    setwd(paste(path.to.extdata,paste0(Province,'/EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/"))
    #Make proposed changes to FileX
    file_x$FIELDS$WSTA<-paste0("WHTE", formatC(width = 4, as.integer((i)), flag = "0"))
    file_x$FIELDS$ID_SOIL<-paste0('TRAN', formatC(width = 5, as.integer((i)), flag = "0"))
    file_x$CULTIVARS$CR <- code
    file_x$CULTIVARS$INGENO <- ingenoid
    ex_profile <- DSSAT::read_sol("SOIL.SOL", id_soil = paste0('TRAN', formatC(width = 5, as.integer((i)),flag = "0")))
    file_x$`INITIAL CONDITIONS`$SH2O<- ex_profile$SDUL #Assume field capacity as initial condition
    file_x$`INITIAL CONDITIONS`$ICBL <- ex_profile$SLB
    file_x$`INITIAL CONDITIONS`$ICDAT <- as.POSIXct(coords$startingDate[i])
    file_x$`PLANTING DETAILS`$PDATE <- as.POSIXct(coords$plantingDate[i]) 
    file_x$`HARVEST DETAILS`$HDATE <- as.POSIXct(coords$harvestDate[i])
    file_x$`SIMULATION CONTROLS`$SDATE <- as.POSIXct(coords$startingDate[i])
    file_x$`SIMULATION CONTROLS`$NYERS <- number_years
    file_x$`TREATMENTS                        -------------FACTOR LEVELS------------`$TNAME <- paste0("Initial planting")
    for (j in 1:plantingWindow){
      file_x$`INITIAL CONDITIONS`<- file_x$`INITIAL CONDITIONS` %>% add_row(!!!file_x$`INITIAL CONDITIONS`[file_x$`INITIAL CONDITIONS`$C==1,])
      file_x$`INITIAL CONDITIONS`[1+j,]$C <- 1+j
      file_x$`INITIAL CONDITIONS`[1+j,]$ICDAT <- as.POSIXct(coords$startingDate[i]) %m+% weeks(j)
      
      file_x$`PLANTING DETAILS` <- file_x$`PLANTING DETAILS` %>% add_row(!!!file_x$`PLANTING DETAILS`[file_x$`PLANTING DETAILS`$P==1,])
      file_x$`PLANTING DETAILS`[1+j,]$P <- 1+j
      file_x$`PLANTING DETAILS`[1+j,]$PDATE <- as.POSIXct(coords$plantingDate[i]) %m+% weeks(j)
      
      
      file_x$`HARVEST DETAILS` <- file_x$`HARVEST DETAILS` %>% add_row(!!!file_x$`HARVEST DETAILS`[file_x$`HARVEST DETAILS`$H==1,])
      file_x$`HARVEST DETAILS`[1+j,]$HDATE <- as.POSIXct(coords$harvestDate[i]) %m+% weeks(j)
      file_x$`HARVEST DETAILS`[1+j,]$H <- 1+j
      
      file_x$`SIMULATION CONTROLS`<- file_x$`SIMULATION CONTROLS` %>% add_row(!!!file_x$`SIMULATION CONTROLS`[file_x$`SIMULATION CONTROLS`$N==1,])
      file_x$`SIMULATION CONTROLS`[1+j,]$N <- 1+j
      file_x$`SIMULATION CONTROLS`[1+j,]$SDATE <- as.POSIXct(coords$startingDate[i]) %m+% weeks(j)
        
      file_x$`TREATMENTS                        -------------FACTOR LEVELS------------` <- file_x$`TREATMENTS                        -------------FACTOR LEVELS------------` %>% 
        add_row(!!!file_x$`TREATMENTS                        -------------FACTOR LEVELS------------`[file_x$`TREATMENTS                        -------------FACTOR LEVELS------------`$N==1,])
      file_x$`TREATMENTS                        -------------FACTOR LEVELS------------`[1+j,]$N <- 1+j
      file_x$`TREATMENTS                        -------------FACTOR LEVELS------------`[1+j,]$TNAME <- paste0("Planting + ", j ,"weeks")
      file_x$`TREATMENTS                        -------------FACTOR LEVELS------------`[1+j,]$IC <- 1+j
      file_x$`TREATMENTS                        -------------FACTOR LEVELS------------`[1+j,]$MP <- 1+j
      file_x$`TREATMENTS                        -------------FACTOR LEVELS------------`[1+j,]$MH <- 1+j
      file_x$`TREATMENTS                        -------------FACTOR LEVELS------------`[1+j,]$SM <- 1+j
      }

    #Overwrite original FileX with new values
    DSSAT::write_filex(file_x,paste0('EXTE', formatC(width = 4, as.integer((i)), flag = "0"),'.',code,'X'))
  }else{
    setwd(paste(path.to.extdata,paste0('EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/"))
    #Make proposed changes to FileX
    file_x$FIELDS$WSTA <- paste0("WHTE", formatC(width = 4, as.integer((i)), flag = "0"))
    file_x$FIELDS$ID_SOIL<-paste0('TRAN', formatC(width = 5, as.integer((i)), flag = "0"))
    file_x$CULTIVARS$CR <- code
    file_x$`PLANTING DETAILS`$PDATE <- as.POSIXct(coords$plantingDate[i])
    file_x$`INITIAL CONDITIONS`$ICDAT <- as.POSIXct(coords$startingDate[i])  #Meanwhile the same date than the planting date## this is changed to a month prior to planting, right??
    file_x$`SIMULATION CONTROLS`$SDATE <- as.POSIXct(coords$startingDate[i])
    file_x$`HARVEST DETAILS`$HDATE <- as.POSIXct(coords$harvestDate[i])
    #file_x$`TREATMENTS                        -------------FACTOR LEVELS------------`$TNAME <- paste0("Trial planting")
    
    ex_profile <- suppressWarnings(DSSAT::read_sol("SOIL.SOL", id_soil = paste0('TRAN', formatC(width = 5, as.integer((i))," ", flag = "0"))))
    file_x$`INITIAL CONDITIONS`$SH2O<- ex_profile$SDUL #Assume field capacity as initial condition
    file_x$`INITIAL CONDITIONS`$ICBL <- ex_profile$SLB
    #Overwrite original FileX with new values
    DSSAT::write_filex(file_x,paste0('EXTE', formatC(width = 4, as.integer((i)), flag = "0"),'.',code,'X'))
    
  }
  gc()
} 



#' Create multiple experimental files
#'
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param filex_temp Name of the template experimental file in DSSAT format (FILEX)
#' @param Planting_month_date is needed only for AOI and should be provided as month_date, for trial locations the actual planting date is be used so no need to change the default value
#' @param Harvest_month_date if AOI is TRUE, Harvest_month_date should be provided in mm-dd format.  weather data across years between Planting_month_date and Harvest_month_date will be provided
#' @param ID trial ID
#' @param season when data is needed for more than one season, this needs to be provided to be used in the file name
#' @param plantingWindow number of weeks starting considering the Planting_month_date as earliest planting week. It is given when several planting dates are to be tested to determine optimal planting date

#' @return
#' @export
#'
#' @examples dssat.expfile(country = "Rwanda",  useCaseName = "RAB", Crop = "Maize", AOI = FALSE, filex_temp="MZRL8142.MZX", Planting_month_date = NULL,jobs=10)


dssat.expfile <- function(country, useCaseName, Crop, AOI = FALSE,filex_temp, Planting_month_date=NULL,Harvest_month_date=NULL, ID="TLID",season =NULL, plantingWindow=1, ingenoid, Province){  #xmin,xmax,ymin,ymax,res,jobs,ex.name,path.to.extdata){
  if(AOI == TRUE){
    if(is.null(Planting_month_date) | is.null(Harvest_month_date)){
      print("with AOI=TRUE, Planting_month_date, Harvest_month_date can not be null, please refer to the documentation and provide mm-dd for both parameters")
      return(NULL)
    }
    countryCoord <- readRDS(paste("~/agwise-datacuration/dataops/datacuration/Data/useCase_", country, "_",useCaseName, "/", Crop, "/result/AOI_GPS.RDS", sep=""))
    
    countryCoord <- unique(countryCoord[, c("lon", "lat")])
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    
    ## check if both planting and harvest dates are in the same year
    Planting_month <- as.numeric(str_extract(Planting_month_date, "[^-]+"))
    Harvest_month <- as.numeric(str_extract(Harvest_month_date, "[^-]+"))
    
    ## py and hy are used only as place holder for formatting purposes
    if(Planting_month < Harvest_month){
      planting_harvest_sameYear <- TRUE
      py <- 2000
      hy <- 2000
    }else{
      planting_harvest_sameYear <- FALSE
      py <- 2000
      hy <- 2001
    }
    
    ## set planting date one moth prior to the given Planting_month_date so that initial condition for the crop model could be set correctly
    Planting_month_date <- as.Date(paste0(py, "-",Planting_month_date)) ## the year is only a place holder to set planting month 1 month earlier
    countryCoord$plantingDate <- Planting_month_date
    Planting_month_date <- Planting_month_date %m-% months(1)
    
    ## if multiple planting dates are to be tested, adjust the Harvest_month_date to extract weather data for the later planting dates.  
    Harvest_month_date <- as.Date(paste0(hy, "-",Harvest_month_date)) ## the year is only a place holder to set planting month 1 month earlier
    countryCoord$harvestDate <- Harvest_month_date
    if(plantingWindow > 1 & plantingWindow <= 5){
      Harvest_month_date <- Harvest_month_date %m+% months(1)
    }else if(plantingWindow > 5 & plantingWindow <=8){
      Harvest_month_date <- Harvest_month_date %m+% months(2)
    }
    
    countryCoord$startingDate <- Planting_month_date
    countryCoord$endDate <- Harvest_month_date
    
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    names(countryCoord) <- c("longitude", "latitude","plantingDate", "harvestDate", "startingDate", "endDate")
    ground <- countryCoord
    
  }else{
    GPS_fieldData <- readRDS(paste("~/agwise-datacuration/dataops/datacuration/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/compiled_fieldData.RDS", sep=""))  
    #countryCoord <- unique(GPS_fieldData[, c("lon", "lat", "plantingDate", "harvestDate", ID)])
    countryCoord <- unique(GPS_fieldData[, c("lon", "lat", "plantingDate", "harvestDate")])
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    countryCoord$startingDate <- as.Date(countryCoord$plantingDate, "%Y-%m-%d") %m-% months(1)
    #names(countryCoord) <- c("longitude", "latitude", "plantingDate", "harvestDate", "ID","startingDate")
    names(countryCoord) <- c("longitude", "latitude", "plantingDate", "harvestDate","startingDate")
    ground <- countryCoord
  }

  pathIn <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/raw/geo_4cropModel/", sep="")
  
  
  if(AOI == TRUE){
    Rainfall <- readRDS(paste(pathIn, "Rainfall_Season_",season,"_PointData_AOI.RDS", sep=""))
    Soil <- readRDS(paste(pathIn, "SoilDEM_PointData_AOI_profile.RDS", sep=""))

  }else{
    Rainfall <- readRDS(paste(pathIn, "Rainfall_PointData_trial.RDS", sep=""))
    Soil <- readRDS(paste(pathIn, "SoilDEM_PointData_trial_profile.RDS", sep=""))
  }
  
  names(Soil)[names(Soil)=="lat"] <- "latitude"
  names(Soil)[names(Soil)=="lon"] <- "longitude"
  Soil <- na.omit(Soil)
  
  if(AOI == TRUE){
    metaDataWeather <- as.data.frame(Rainfall[,1:7])
  }else{
    metaDataWeather <- as.data.frame(Rainfall[,1:11])
  }
  metaData_Soil <- Soil[,c("longitude", "latitude","NAME_1","NAME_2")]
  
  
  metaData <- merge(metaDataWeather,metaData_Soil)
    if(AOI==TRUE){
      number_years <- max(lubridate::year(as.Date(metaData$startingDate, "%Y-%m-%d")))- min(lubridate::year(as.Date(metaData$startingDate, "%Y-%m-%d")))
      metaData <- unique(metaData[,1:4])
    }else{
      number_years <- 1
      #metaData <- subset(metaData,select=-ID)
      }
  coords <- merge(metaData,ground)
  coords <- coords[coords$NAME_1==Province,]
  grid <- as.matrix(coords)
  #Set working directory to save the results
  if(AOI == TRUE){
    path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/DSSAT/AOI/",ingenoid, sep="")
  }else{
    path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/DSSAT/fieldData/",ingenoid, sep="")
  }
  #Define working directory with template data
  path.to.temdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/Landing/DSSAT", sep="")
  #We need to add more codes
  crops <- c("Maize", "Potato", "Rice", "Soybean", "Wheat")
  cropcode <- c("MZ","PT", "RI", "SB", "WH")
  
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
  
  results <- map(seq_along(grid[,1]), create_filex, path.to.temdata=path.to.temdata, filex_temp=filex_temp, path.to.extdata=path.to.extdata, 
                 coords=coords, AOI=AOI, code=code, plantingWindow=plantingWindow, number_years=number_years, ingenoid=ingenoid, Province =Province) %||% print("Progress:")

}
