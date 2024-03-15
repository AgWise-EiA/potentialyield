# Create DSSAT experimental file

# Introduction: 
# This script allows the creation of experimental files up to administrative level 2 
# Authors : P.Moreno, A. Sila, S. Mkuhlani, E.Bendito Garcia 
# Credentials : EiA, 2024
# Last modified March 13, 2024 by P.Moreno 


#################################################################################################################
## sourcing required packages 
#################################################################################################################
packages_required <- c("tidyverse", "lubridate","DSSAT")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  if(packages_required[!installed_packages] =="DSSAT"){
    remotes::install_github("palderman/DSSAT", ref = "develop",force=T)
  } else {
    install.packages(packages_required[!installed_packages])
  }
}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))

#' Create one experimental file (repetitive function)
#'
#' @param i point/folder from a list
#' @param path.to.temdata directory with template weather and soil data in DSSAT format
#' @param filex_temp Name of the template experimental file in DSSAT format (FILEX)
#' @param path.to.extdata working directory to save the weather and soil data in DSSAT format
#' @param coords dataframe with the locations and metadata (created by the function dssat.expfile)
#' @param AOI "Area of interest" AOI=TRUE when we want to explore crop simulations with historical data.
#'        AOI= FALSE when there is information for actual trial sites (with observed yield data).
#' @param crop_code crop code in DSSAT format (e.g. "MZ" for maize, created by function dssat.expfile)
#' @param plantingWindow number of weeks that define the planting window considering the Planting_month_date as the earliest planting week. 
#'        It is given when several planting dates are to be tested to determine optimal planting date (applies when AOI= TRUE)  
#' @param number_years Number of years the simulations are run when AOI=TRUE (it does not apply to the trial location data)
#' @param varietyid id of the variety based on the cultivar file of DSSAT (column @VAR# in the cultivar file and parameter INGENO in the experimental file *.**X)
#' @param zone Name of the administrative level 1 for the specific location the experimental file is created.
#' @param level2 Name of the administrative level 2 (has to be part of the administrative level 1 or "zone" of the country) 
#'        for the specific location the experimental file is created
#' 
#' @return
#'
#' @examples create_filex(i=1,path.to.temdata = "/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_Kenya_KALRO/Maize/Landing/DSSAT",
#'                        filex_temp="KEAG8104.MZX",
#'                        path.to.extdata = "/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_Kenya_Solidaridad/Maize/transform/DSSAT/AOI/999991",
#'                        country="Kenya",coords = , AOI=TRUE, crop_code ="MZ",plantingWindow=1,number_years,varietyid = "999991", zone ="Tete", level2="Macanga")




create_filex <-function(i,path.to.temdata,filex_temp,path.to.extdata,coords, AOI=TRUE, crop_code,plantingWindow=1,number_years,varietyid, zone, level2){
  setwd(path.to.temdata)
  
  #Read in original FileX
  file_x <- DSSAT::read_filex(filex_temp)
  #Set the experimental directory
  if(AOI==TRUE){
    #setwd(paste(path.to.extdata,"AOI",paste0('EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/"))
    setwd(paste(path.to.extdata,paste0(zone,'/EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/"))
    #Make proposed changes to FileX
    file_x$FIELDS$WSTA<-paste0("WHTE", formatC(width = 4, as.integer((i)), flag = "0"))
    file_x$FIELDS$ID_SOIL<-paste0('TRAN', formatC(width = 5, as.integer((i)), flag = "0"))
    file_x$CULTIVARS$CR <- crop_code
    file_x$CULTIVARS$INGENO <- varietyid
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
    DSSAT::write_filex(file_x,paste0('EXTE', formatC(width = 4, as.integer((i)), flag = "0"),'.',crop_code,'X'))
  }else{
    setwd(paste(path.to.extdata,paste0('EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/"))
    #Make proposed changes to FileX
    file_x$FIELDS$WSTA <- paste0("WHTE", formatC(width = 4, as.integer((i)), flag = "0"))
    file_x$FIELDS$ID_SOIL<-paste0('TRAN', formatC(width = 5, as.integer((i)), flag = "0"))
    file_x$CULTIVARS$CR <- crop_code
    file_x$`PLANTING DETAILS`$PDATE <- as.POSIXct(coords$plantingDate[i])
    file_x$`INITIAL CONDITIONS`$ICDAT <- as.POSIXct(coords$startingDate[i])  #Meanwhile the same date than the planting date## this is changed to a month prior to planting, right??
    file_x$`SIMULATION CONTROLS`$SDATE <- as.POSIXct(coords$startingDate[i])
    file_x$`HARVEST DETAILS`$HDATE <- as.POSIXct(coords$harvestDate[i])
    #file_x$`TREATMENTS                        -------------FACTOR LEVELS------------`$TNAME <- paste0("Trial planting")
    
    ex_profile <- suppressWarnings(DSSAT::read_sol("SOIL.SOL", id_soil = paste0('TRAN', formatC(width = 5, as.integer((i))," ", flag = "0"))))
    file_x$`INITIAL CONDITIONS`$SH2O<- ex_profile$SDUL #Assume field capacity as initial condition
    file_x$`INITIAL CONDITIONS`$ICBL <- ex_profile$SLB
    #Overwrite original FileX with new values
    DSSAT::write_filex(file_x,paste0('EXTE', formatC(width = 4, as.integer((i)), flag = "0"),'.',crop_code,'X'))
    
  }
  gc()
} 



#' Create multiple experimental files
#'
#' @param country country name
#' @param useCaseName use case name 
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI "Area of interest" AOI=TRUE when we want to explore crop simulations with historical data.
#'        AOI= FALSE when there is information for actual trial sites (with observed yield data).
#' @param filex_temp Name of the template experimental file in DSSAT format (FILEX)
#' @param Planting_month_date it is needed only when AOI=TRUE and it should be provided as mm-dd format 
#' @param Harvest_month_date if AOI =TRUE, Harvest_month_date is the initial month for harvesting and it should be provided in mm-dd format.
#'        The parameter is no needed it when AOI=FALSE because the actual harvesting date from the trials would be provided.   
#' @param ID trial ID
#' @param season when data is needed for more than one season, this needs to be provided to be used in the file name
#' @param plantingWindow number of weeks starting considering the Planting_month_date as earliest planting week. It is given when several 
#'        planting dates are to be tested to determine optimal planting date
#' @param varietyid ID of the variety based on the cultivar file of DSSAT (column @VAR# in the cultivar file and parameter INGENO in the experimental file *.**X)
#' @param zone Name of administrative level 1 for the specific location the experimental file is created
#' @param level2 Name of administrative level 2 (part of the administrative level 1 or "zone") for the specific location the experimental file is created
#'
#' @return
#' @export
#'
#' @examples dssat.expfile(country="Mozambique", useCaseName = "Solidaridad", Crop = "Maize", AOI = FALSE,
#'                         filex_temp= "ELZA0201.MZX", Planting_month_date=NULL,Harvest_month_date=NULL, 
#'                         ID="TLID",season = 1, plantingWindow = 1,varietyid = "999991", zone ="Tete", level2="Macanga")

dssat.expfile <- function(country, useCaseName, Crop, AOI = TRUE,filex_temp, Planting_month_date=NULL,Harvest_month_date=NULL, 
                          ID="TLID",season =1, plantingWindow=1,varietyid, zone, level2){  
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
    countryCoord <- unique(GPS_fieldData[, c("lon", "lat", "plantingDate", "harvestDate")])
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    countryCoord$startingDate <- as.Date(countryCoord$plantingDate, "%Y-%m-%d") %m-% months(1)
    names(countryCoord) <- c("longitude", "latitude", "plantingDate", "harvestDate","startingDate")
    ground <- countryCoord
  }

  pathIn <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/raw/geo_4cropModel/", sep="")

  if(AOI == TRUE){
    Rainfall <- readRDS(paste(pathIn,zone, "/Rainfall_Season_", season, "_PointData_AOI.RDS", sep=""))
    Soil <- readRDS(paste(pathIn,zone,"/SoilDEM_PointData_AOI_profile.RDS", sep=""))

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
      }
  coords <- merge(metaData,ground)
  coords <- coords[coords$NAME_1==zone,]
  grid <- as.matrix(coords)
  
  #Set working directory to save the results
  if(AOI == TRUE){
    path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/DSSAT/AOI/",varietyid, sep="")
  }else{
    path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/DSSAT/fieldData/",varietyid, sep="")
  }
  #Define working directory with template data
  path.to.temdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/Landing/DSSAT", sep="")
  #We need to add more codes
  crops <- c("Maize", "Potato", "Rice", "Soybean", "Wheat")
  cropcode_supported <- c("MZ","PT", "RI", "SB", "WH")
  
  cropid <- which(crops == Crop)
  crop_code <- cropcode_supported[cropid]
  

  setwd(path.to.extdata)

  
  results <- map(seq_along(grid[,1]), create_filex, path.to.temdata=path.to.temdata, filex_temp=filex_temp, path.to.extdata=path.to.extdata, 
                 coords=coords, AOI=AOI, crop_code=crop_code, plantingWindow=plantingWindow, number_years=number_years, varietyid=varietyid, 
                 zone=zone, level2=level2) %||% print("Progress:")

}

