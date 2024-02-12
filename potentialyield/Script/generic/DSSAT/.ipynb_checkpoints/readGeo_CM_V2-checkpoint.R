
#################################################################################################################
## sourcing required packages
#################################################################################################################
packages_required <- c("doParallel", "foreach", "chirps", "tidyverse", "dplyr", "lubridate", "stringr","sf","purrr","DSSAT")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))

##### Equations from apsimx package
#' @description Texture triangle as equations
#' @details It requires the silt and clay percentages to define the texture class
#
#' Title getting the texture class
#'
#' @param usda_clay percentage of clay (as index or /100)
#' @param usda_silt percentage of silt (as index or /100)
#' @return class (texture class)
#' @examples texture_class(clay,silt)
#'
texture_class <- function (usda_clay, usda_silt ) {
  
  if(usda_clay < 0 || usda_clay > 1) stop("usda_clay should be between 0 and 1")
  if(usda_silt < 0 || usda_silt > 1) stop("usda_silt should be between 0 and 1")
  
  intl_clay <- usda_clay
  intl_silt <- usda_silt
  intl_sand <- 1.0 - intl_clay - intl_silt
  
  if ((intl_sand < 0.75 - intl_clay) && (intl_clay >= 0.40)) {
    class <- "silty clay"
  } else if ((intl_sand < 0.75 - intl_clay) && (intl_clay >= 0.26)) {
    class <- "silty clay loam"
  } else if (intl_sand < 0.75 - intl_clay) {
    class <- "silty loam"
  } else if ((intl_clay >= 0.40 + (0.305-0.40)/(0.635-0.35) * (intl_sand-0.35)) && (intl_clay < 0.50 + (0.305-0.50)/(0.635-0.50) * (intl_sand - 0.50))) {
    class <- "clay"
  } else if (intl_clay >= 0.26 + (0.305-0.26)/(0.635-0.74) * (intl_sand-0.74)) {
    class <- "sandy clay"
  } else if ((intl_clay >= 0.26 + (0.17-0.26)/(0.83-0.49) * (intl_sand-0.49)) && (intl_clay < 0.10 + (0.305-0.10)/(0.635-0.775) * (intl_sand - 0.775))) {
    class <- "clay loam"
  } else if (intl_clay >= 0.26 + (0.17-0.26)/(0.83-0.49) * (intl_sand-0.49)) {
    class <- "sandy clay loam"
  } else if ((intl_clay >= 0.10 + (0.12-0.10)/(0.63-0.775) * (intl_sand-0.775)) && (intl_clay < 0.10 + (0.305-0.10)/(0.635-0.775) * (intl_sand - 0.775))) {
    class <- "loam"
  } else if (intl_clay >= 0.10 + (0.12-0.10)/(0.63-0.775) * (intl_sand-0.775)) {
    class <- "sandy loam"
  } else if (intl_clay < 0.00 + (0.08-0.00)/(0.88-0.93) * (intl_sand-0.93)) {
    class <- "loamy sand"
  } else {
    class <- "sand"
  }
  
  return( class )
}

#' Evaporation limit function from Ritchie et al. (1989); cited in Allen et al. (2005)
#' @param clay1 Clay percentage for the top soil horizon
#' @param sand1 Sand percentage for the top soil horizon
#' @keywords internal
#' @export
slu1 <- function(clay1,sand1) {
  ifelse(sand1>=80, (20-0.15*sand1),
         ifelse(clay1>=50,(11-0.06*clay1),
                (8-0.08*clay1)))
}



#' Function that creates the soil and weather file for one location/folder
#'
#' @param i last digits of the folder (folder ID)
#' @param country country name
#' @param path.to.extdata working directory to save the weather and soil data in DSSAT format
#' @param path.to.temdata directory with template weather and soil data in DSSAT format
#' @param Tmaxdata dataframe with the maximum data for all the locations
#' @param Tmindata dataframe with the minimum temperature data for all the locations
#' @param Sraddata dataframe with the solar radiation data for all the locations
#' @param Rainfalldata dataframe with the rainfall data for all the locations
#' @param RelativeHum dataframe with the relative humidity data for all the locations
#' @param coords dataframe with the locations and metadata
#' @param Soil dataframe with the soil data information
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @return soil and weather file in DSSAT format
#' @export
#'
#' @examples process_grid_element(1)

process_grid_element <- function(i,country,path.to.extdata,path.to.temdata,Tmaxdata,Tmindata,Sraddata,Rainfalldata,coords,Soil,AOI,Province) {
  
  if(AOI==TRUE & !is.na(Province)){
    if (!dir.exists(file.path(paste(path.to.extdata,"AOI/",paste0(Province,'/EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/")))){
      dir.create(file.path(paste(path.to.extdata,"AOI/",paste0(Province,'/EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/")), recursive = TRUE)
    }
    setwd(paste(path.to.extdata,"AOI/",paste0(Province,'/EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/"))
    
  }else{
    if (!dir.exists(file.path(paste(path.to.extdata,paste0('EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/")))){
      dir.create(file.path(paste(path.to.extdata,paste0('EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/")), recursive = TRUE)
    }
    setwd(paste(path.to.extdata,paste0('EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/"))
  }
  Tmaxdata <- Tmaxdata[Tmaxdata$longitude==coords$longitude[i] & Tmaxdata$latitude==coords$latitude[i],]
  Tmindata <- Tmindata[Tmindata$longitude==coords$longitude[i] & Tmindata$latitude==coords$latitude[i],]
  Sraddata <- Sraddata[Sraddata$longitude==coords$longitude[i] & Sraddata$latitude==coords$latitude[i],]
  Rainfalldata <- Rainfalldata[Rainfalldata$longitude==coords$longitude[i] & Rainfalldata$latitude==coords$latitude[i],]
  #RelativeHum <- RelativeHum[RelativeHum$longitude==coords$longitude[i] & RelativeHum$latitude==coords$latitude[i],]
  
  #print("i am here")
  if(AOI == TRUE){
    Rainfalldata <- pivot_longer(Rainfalldata,
                                 cols=-c("NAME_1","NAME_2","startingDate", "endDate","ID", "point"),
                                 names_to = c("Variable", "Date"),
                                 names_sep = "_",
                                 values_to = "RAIN")
    Rainfalldata <-unique(select(Rainfalldata,-c(Variable,startingDate, endDate)))
    
    Sraddata <- pivot_longer(Sraddata,
                             cols=-c("NAME_1","NAME_2","startingDate", "endDate","ID","point"),
                             names_to = c("Variable", "Date"),
                             names_sep = "_",
                             values_to = "SRAD")
    Sraddata <-unique(select(Sraddata,-c(Variable,startingDate, endDate)))
    
    Tmaxdata <- pivot_longer(Tmaxdata,
                             cols=-c("NAME_1","NAME_2","startingDate", "endDate","ID","point"),
                             names_to = c("Variable", "Date"),
                             names_sep = "_",
                             values_to = "TMAX")
    Tmaxdata <-unique(select(Tmaxdata,-c(Variable,startingDate, endDate)))
    
    Tmindata <- pivot_longer(Tmindata,
                             cols=-c("NAME_1","NAME_2","startingDate", "endDate","ID","point"),
                             names_to = c("Variable", "Date"),
                             names_sep = "_",
                             values_to = "TMIN")
    Tmindata <-unique(select(Tmindata,-c(Variable,startingDate, endDate)))
    
    # RelativeHum <- pivot_longer(RelativeHum,
    #                             cols=-1:-7,
    #                             names_to = c("Variable", "Date"),
    #                             names_sep = "_",
    #                             values_to = "RHUM")
    # RelativeHum <-unique(select(RelativeHum,-c(Variable,startingDate, endDate)))
  }else{
    Rainfalldata <- pivot_longer(Rainfalldata,
                                 cols=-1:-11,
                                 names_to = c("Variable", "Date"),
                                 names_sep = "_",
                                 values_to = "RAIN")
    Rainfalldata <-select(Rainfalldata,-Variable)
    
    Sraddata <- pivot_longer(Sraddata,
                             cols=-1:-11,
                             names_to = c("Variable", "Date"),
                             names_sep = "_",
                             values_to = "SRAD")
    Sraddata <-select(Sraddata,-Variable)
    
    Tmaxdata <- pivot_longer(Tmaxdata,
                             cols=-1:-11,
                             names_to = c("Variable", "Date"),
                             names_sep = "_",
                             values_to = "TMAX")
    Tmaxdata <-select(Tmaxdata,-Variable)
    
    Tmindata <- pivot_longer(Tmindata,
                             cols=-1:-11,
                             names_to = c("Variable", "Date"),
                             names_sep = "_",
                             values_to = "TMIN")
    Tmindata <-select(Tmindata,-Variable)
  }
  
  # RelativeHum <- pivot_longer(RelativeHum,
  #                             cols=-1:-11,
  #                             names_to = c("Variable", "Date"),
  #                             names_sep = "_",
  #                             values_to = "RHUM")
  # RelativeHum <-select(RelativeHum,-Variable)
  
  tst <- na.omit(merge(Tmaxdata, merge(Tmindata,merge(Sraddata,Rainfalldata))))
  tst$DATE <- as.POSIXct(tst$Date, format = "%Y-%m-%d", tz = "UTC")
  tst <- select(tst,c(DATE,TMAX,TMIN,SRAD,RAIN))
  tst  <- mutate(tst , across(c(TMAX,TMIN,SRAD,RAIN), as.numeric))
  
  # Calculate long-term average temperature (TAV)
  tav <- tst %>%
    dplyr::summarise(TAV=mean((TMAX+TMIN)/2,na.rm=T))
  
  # Calculate monthly temperature amplitude (AMP)
  amp <- tst %>%
    # Extract month from DATE column
    mutate(month = lubridate::month(as.Date(tst$DATE,format = "%y%j"))) %>%
    # Group data by month
    group_by(month) %>%
    # Calculate monthly means
    dplyr::summarise(monthly_avg = mean((TMAX+TMIN)/2,na.rm=T)) %>%
    # Calculate AMP as half the difference between minimum and
    #     maximum monthly temperature
    dplyr::summarise(AMP = (max(monthly_avg, na.rm=T)-min(monthly_avg,na.rm=T))/2)
  
  
  #   #Get elevation
  elev <- Soil$altitude[which(Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]))]
  elev <-ifelse(length(elev) >0,
                Soil$altitude[which(Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]))],
                -99)
  #   #Location (NAME_2)
  INS <- toupper(substr(unique(Tmaxdata$NAME_2),start =1, stop =4))
  # Generate new general information table
  general_new <- tibble(
    INSI = INS,
    LAT = as.numeric(coords[i, 2]),
    LONG = as.numeric(coords[i, 1]),
    ELEV = elev,
    TAV = tav,
    AMP = amp,
    REFHT = 2,
    WNDHT = 2
  )
  
  #   # Add station information
  attr(tst, "GENERAL") <- general_new
  
  DSSAT::write_wth(tst, paste0("WHTE", formatC(width = 4, (as.integer(i)), flag = "0"), ".WTH"))
  #cat(" Writing weather file")
  
  #   ##########################################
  # Get soil ISRIC data from server
  Depth<-c(5,15,30,60,100,200)
  LL15 <-as.numeric(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),c("PWP_0-5cm","PWP_5-15cm","PWP_15-30cm","PWP_30-60cm","PWP_60-100cm","PWP_100-200cm")])
  DUL  <-as.numeric(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),c("FC_0-5cm","FC_5-15cm","FC_15-30cm","FC_30-60cm","FC_60-100cm","FC_100-200cm")])
  SAT  <-as.numeric(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),c("SWS_0-5cm","SWS_5-15cm","SWS_15-30cm","SWS_30-60cm","SWS_60-100cm","SWS_100-200cm")])
  SKS  <-as.numeric(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),c("KS_0-5cm","KS_5-15cm","KS_15-30cm","KS_30-60cm","KS_60-100cm","KS_100-200cm")])/10
  SSS  <-round(as.numeric(SKS), digits = 1)
  BDM  <- as.numeric(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),c("bdod_0-5cm","bdod_5-15cm","bdod_15-30cm","bdod_30-60cm","bdod_60-100cm","bdod_100-200cm")])
  LOC  <- as.numeric((Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),c("soc_0-5cm","soc_5-15cm","soc_15-30cm","soc_30-60cm","soc_60-100cm","soc_100-200cm")])/10)
  LCL  <- as.numeric(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),c("clay_0-5cm","clay_5-15cm","clay_15-30cm","clay_30-60cm","clay_60-100cm","clay_100-200cm")])
  LSI  <- as.numeric(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),c("silt_0-5cm","silt_5-15cm","silt_15-30cm","silt_30-60cm","silt_60-100cm","silt_100-200cm")])
  LNI  <- c(as.numeric((Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),c("nitrogen_0-5cm","nitrogen_5-15cm","nitrogen_15-30cm","nitrogen_30-60cm","nitrogen_60-100cm","nitrogen_100-200cm")])/10))
  LHW  <- as.numeric(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),c("phh2o_0-5cm","phh2o_5-15cm","phh2o_15-30cm","phh2o_30-60cm","phh2o_60-100cm","phh2o_100-200cm")])
  LDR <- as.numeric(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),c("LDR")])
  CEC <- as.numeric(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),c("cec_0-5cm","cec_5-15cm","cec_15-30cm","cec_30-60cm","cec_60-100cm","cec_100-200cm")])
  
  
  #   ##### Runoff curve no. [Soil Conservation Service/NRCS] #####
  texture <- texture_class((LCL[1]/100), (LSI[1]/100))
  
  textureClasses <- c("clay", "silty clay", "sandy clay", "clay loam", "silty clay loam", "sandy clay loam", "loam", "silty loam", "sandy loam", "silt", "loamy sand", "sand", "NO DATA")
  textureClasses_sum <- c("C", "SIC", "SC", "CL", "SICL", "SCL", "L", "SIL", "SL", "SI", "LS", "S", "NO DATA")
  
  Albedo <- c(0.12, 0.12, 0.13, 0.13, 0.12, 0.13, 0.13, 0.14, 0.13, 0.13, 0.16, 0.19, 0.13)
  CN2 <- c(73.0, 73.0, 73.0, 73.0, 73.0, 73.0, 73.0, 73.0, 68.0, 73.0, 68.0, 68.0, 73.0)
  SWCON <- c(0.25, 0.3, 0.3, 0.4, 0.5, 0.5, 0.5, 0.5, 0.6, 0.5, 0.6, 0.75, 0.5)
  
  wtc <- which(textureClasses == texture)
  #Soil albedo
  ALB <- Albedo[wtc]
  #Runoff curve
  LRO <- CN2[wtc]
  
  texture_soil <- textureClasses_sum[wtc]
  
  
  SLU <- slu1(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),c("clay_0-5cm")],Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),c("sand_0-5cm")])
  
  #Soil root growth factor. Based on formula from DSSAT. Maybe not the best option for soils with duripan or other root growth limitations
  layer_center <- c(Depth[1]/2, (Depth[-1] - Depth[-length(Depth)]) / 2 + Depth[-length(Depth)])
  RGF = ifelse(Depth<=15, 1,1 * exp(-0.02 * layer_center))
  
  ex_profile <- suppressWarnings(DSSAT::read_sol(paste(path.to.temdata, "soil.sol", sep="/"), id_soil = "IBPN910025"))
  
  
  soilid <- ex_profile %>%
    mutate(PEDON=paste0('TRAN', formatC(width = 5, (as.integer(i)), flag = "0")),
           SOURCE = "ISRIC V2",
           TEXTURE = texture_soil,
           DESCRIPTION = texture,
           SITE= substr(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),c("NAME_2")],start=1,stop=6),
           COUNTRY = country,
           LAT = as.numeric(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),c("latitude")]),
           LONG = as.numeric(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),c("longitude")]),
           SALB = list(ALB),
           SLU1 = list(SLU),
           SLRO = list(LRO),
           SLDR = list(LDR),
           SLB = list(Depth),
           SLMH = list(rep(-99,6)), #No data about the master horizon
           SLLL=list(LL15),
           SSAT=list(SAT),
           SDUL=list(DUL),
           SSKS=list(SSS),
           SBDM=list(BDM),
           SLOC=list(LOC),
           SLCL=list(LCL),
           SLSI=list(LSI),
           SLNI=list(LNI),
           SLHW=list(LHW),
           SCEC=list(CEC),
           SRGF =list(RGF))
  
  DSSAT::write_sol(soilid, 'SOIL.SOL', append = FALSE) 
  #cat(" Writing soil file")
}


# Reading the weather and soil data for crop model and transforming it to DSSAT format
#'
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param season when data is needed for more than one season, this needs to be provided to be used in the file name

#' @return weather and soil data in DSSAT format
#' @export
#'
#' @examples readGeo_CM(country = "Kenya",  useCaseName = "KALRO", Crop = "Maize", AOI = TRUE, season=1, Province = "Kiambu")
readGeo_CM <- function(country, useCaseName, Crop, AOI = FALSE, season=1, Province){
  cat(Province)
  pathIn <- paste("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName,"/", Crop, "/raw/geo_4cropModel/", sep="")
  if(AOI == TRUE & !is.na(Province)){
    Rainfall <- readRDS(paste(pathIn, "Rainfall_Season_", season, "_PointData_AOI.RDS", sep=""))
    Rainfall <- Rainfall[Rainfall$NAME_1 == Province, ]
    #cat("Rain done")
    SolarRadiation <- readRDS(paste(pathIn, "solarRadiation_Season_", season, "_PointData_AOI.RDS", sep=""))
    SolarRadiation <- SolarRadiation[SolarRadiation$NAME_1 == Province, ]
    #cat("sr done")
    TemperatureMax <- readRDS(paste(pathIn, "temperatureMax_Season_", season, "_PointData_AOI.RDS", sep=""))
    TemperatureMax <- TemperatureMax[TemperatureMax$NAME_1 == Province, ]
    #cat("tm done")
    TemperatureMin <- readRDS(paste(pathIn, "temperatureMin_Season_", season, "_PointData_AOI.RDS", sep=""))
    TemperatureMin <- TemperatureMin[TemperatureMin$NAME_1 == Province, ]
    #cat("tmin done")
    #   RelativeHum <- readRDS(paste(pathIn, "relativeHumidity_Season_", season, "_PointData_AOI.RDS", sep=""))
    # RelativeHum  <- RelativeHum[RelativeHum$NAME_1 == Province, ]
    # cat("rh done")
    Soil <- readRDS(paste(pathIn,"SoilDEM_PointData_AOI_profile.RDS", sep=""))
    Soil <- Soil[Soil$NAME_1 == Province, ]
    z<-which(colnames(Soil) %in% c('altitude','slope','TPI','TRI'))
    #Soil <- na.omit(Soil[,-z])
  }else{
    Rainfall <- readRDS(paste(pathIn, "Rainfall_PointData_trial.RDS", sep=""))
    SolarRadiation <- readRDS(paste(pathIn, "solarRadiation_PointData_trial.RDS", sep=""))
    TemperatureMax <- readRDS(paste(pathIn, "temperatureMax_PointData_trial.RDS", sep=""))
    TemperatureMin <- readRDS(paste(pathIn, "temperatureMin_PointData_trial.RDS", sep=""))
    # RelativeHum <- readRDS(paste(pathIn, "relativeHumidity_PointData_trial.RDS", sep=""))
    Soil <- readRDS(paste(pathIn, "SoilDEM_PointData_trial_profile.RDS", sep=""))
  }
  names(Soil)[names(Soil)=="lat"] <- "latitude"
  names(Soil)[names(Soil)=="lon"] <- "longitude"
  
  
  if(AOI == TRUE){
    metaDataWeather <- as.data.frame(Rainfall[,c("longitude", 'latitude', "startingDate", "endDate", "ID", "NAME_1", "NAME_2")])
  }else{
    metaDataWeather <- as.data.frame(Rainfall[,1:11])
  }
  metaData_Soil <-Soil[,c("longitude", "latitude","NAME_1","NAME_2")]
  
  metaDataWeather$point<-paste0(metaDataWeather$longitude,'_',metaDataWeather$latitude)
  metaData_Soil$point<-paste0(metaData_Soil$longitude,'_',metaData_Soil$latitude)
  Soil$point<-paste0(Soil$longitude,'_',Soil$latitude)
  metaData <- merge(metaDataWeather[,-c(6:7)],metaData_Soil[,-c(1:2)], by='point')
  
  #Keep all the soil data with rainfall data
  Soil <- merge(unique(metaData[,1:6]),Soil[,-c(1:2)], by='point')
  
  # Create latlon point for rainfall, solarradiation, tmax amd tmin
  Rainfall$point<-paste0(Rainfall$longitude,'_',Rainfall$latitude)
  SolarRadiation$point<-paste0( SolarRadiation$longitude,'_', SolarRadiation$latitude)
  TemperatureMax$point<-paste0(TemperatureMax$longitude,'_',TemperatureMax$latitude)
  TemperatureMin$point<-paste0(TemperatureMin$longitude,'_',TemperatureMin$latitude)

  #Keep all the weather data that has soil data
  Rainfall <- merge(metaData[,1:3],Rainfall[,-c(1:2)], by='point')
  SolarRadiation <- merge(metaData[,1:3],SolarRadiation[,-c(1:2)], by='point')
  TemperatureMax <- merge(metaData[,1:3],TemperatureMax[,-c(1:2)], by='point')
  TemperatureMin <- merge(metaData[,1:3],TemperatureMin[,-c(1:2)], by='point')
  # RelativeHum <- merge(metaData,RelativeHum)
  
  #return(list(Rainfall, SolarRadiation, TemperatureMax, TemperatureMin,Soil,metaData))
  jobs=1
  cls <- parallel::makePSOCKcluster(jobs)
  doParallel::registerDoParallel(cls)
  # Set working directory to save the results
  path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/DSSAT", sep="")
  
  #Define working directory with template data
  path.to.temdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/Landing/DSSAT", sep="")
  
  
  
  if (!dir.exists(path.to.extdata)){
    dir.create(file.path(path.to.extdata), recursive = TRUE)
  }
  setwd(path.to.extdata)
  
  
  if(AOI==TRUE){
    coords <- unique(metaData[,c("longitude","latitude")])
  }else{
    coords <- metaData
  }
  
  grid <- as.matrix(coords)
  
  # 
  # path.to.extdata=path.to.extdata; path.to.temdata=path.to.temdata; Tmaxdata=TemperatureMax; Tmindata=TemperatureMin; Sraddata=SolarRadiation;
  # # # Rainfalldata=Rainfall; RelativeHum=RelativeHum
  # return(list())
  
  results <- map(seq_along(grid[,1]), process_grid_element, country=country, path.to.extdata=path.to.extdata,
                 path.to.temdata=path.to.temdata, Tmaxdata=TemperatureMax, Tmindata=TemperatureMin, Sraddata=SolarRadiation,
                 Rainfalldata=Rainfall, coords=coords, Soil=Soil, AOI=AOI, Province= Province) %||% print("Progress:")
}

# readGeo_CM(country = "Kenya",  useCaseName = "KALRO", Crop = "Maize", AOI = TRUE, season=1, Province = "Kwale")
#process_grid_element(i,country,path.to.extdata,path.to.temdata,Tmaxdata,Tmindata,Sraddata,Rainfalldata,RelativeHum,coords,Soil,AOI = TRUE, #Province = "Kwale")













# country='Kenya'
# useCaseName='KALRO'
# Crop='Maize'
# Province='Vihiga'
# season=1
# AOI = TRUE
# 
# pathIn <- paste("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName,"/", Crop, "/raw/geo_4cropModel/", sep="")
# Rainfall <- readRDS(paste(pathIn, "Rainfall_Season_", season, "_PointData_AOI.RDS", sep=""))
# Rainfall <- Rainfall[Rainfall$NAME_1 == Province, ]
# Soil <- readRDS(paste(pathIn,"SoilDEM_PointData_AOI_profile.RDS", sep=""))
# Soil <- Soil[Soil$NAME_1 == Province, ]
# 
# names(Soil)[names(Soil)=="lat"] <- "latitude"
# names(Soil)[names(Soil)=="lon"] <- "longitude"
# z<-which(colnames(Soil) %in% c('altitude','slope','TPI','TRI'))
# Soil <- na.omit(Soil[,-z])
# 
# metaDataWeather <- as.data.frame(Rainfall[,c("longitude", 'latitude', "startingDate", "endDate", "ID", "NAME_1", "NAME_2")])
# 
# metaData_Soil <-Soil[,c("longitude", "latitude","NAME_1","NAME_2")]
# 
# metaDataWeather$point<-paste0(metaDataWeather$longitude,'_',metaDataWeather$latitude)
# metaData_Soil$point<-paste0(metaData_Soil$longitude,'_',metaData_Soil$latitude)
# Soil$point<-paste0(Soil$longitude,'_',Soil$latitude)
# metaData <- merge(metaDataWeather[,-c(6:7)],metaData_Soil[,-c(1:2)], by='point')
# 
# #Keep all the soil data with rainfall data
# Soil <- merge(unique(metaData[,1:6]),Soil[-c(1:2)], by='point')
# country = "Kenya";
# useCaseName = "KALRO";
# Crop = "MAIZE";
# AOI = TRUE;
# season = 1;
# Province = "Vihiga"

























































