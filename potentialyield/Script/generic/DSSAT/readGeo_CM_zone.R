# Create weather and soil files in DSSAT format

# Introduction: 
# This script allows the creation of weather and soil files up to administrative level 2
# Authors : P.Moreno, A. Sila, S. Mkuhlani, E.Bendito Garcia 
# Credentials : EiA, 2024
# Last modified June 28, 2024 

#################################################################################################################
## sourcing required packages                                                                                  ##
#################################################################################################################
packages_required <- c("chirps", "tidyverse","sf","DSSAT","furrr","future", "future.apply","parallel","sp")

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

process_grid_element <- function(i,country,path.to.extdata,path.to.temdata,Tmaxdata,Tmindata,Sraddata,Rainfalldata,coords,Soil,AOI,varietyid,zone,level2=NA) {

  if(!is.na(level2) & !is.na(zone)){
    pathOUT <- paste(path.to.extdata,paste0(zone,'/',level2,'/EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/")
  }else if(is.na(level2) & !is.na(zone)){
    pathOUT <- paste(path.to.extdata,paste0(zone,'/EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/")
  }else if(!is.na(level2) & is.na(zone)){
    print("You need to define first a zone (administrative level 1) to be able to get data for level 2 (administrative level 2) in the creation of soil and weather files. Process will stop")
    return(NULL)
  }else{
    pathOUT <- paste(path.to.extdata,paste0('EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/")
  }
  if (!dir.exists(file.path(pathOUT))){
    dir.create(file.path(pathOUT), recursive = TRUE)
  }
  setwd(pathOUT)

  Tmaxdata <- Tmaxdata[Tmaxdata$longitude==coords$longitude[i] & Tmaxdata$latitude==coords$latitude[i],]
  Tmindata <- Tmindata[Tmindata$longitude==coords$longitude[i] & Tmindata$latitude==coords$latitude[i],]
  Sraddata <- Sraddata[Sraddata$longitude==coords$longitude[i] & Sraddata$latitude==coords$latitude[i],]
  Rainfalldata <- Rainfalldata[Rainfalldata$longitude==coords$longitude[i] & Rainfalldata$latitude==coords$latitude[i],]
 
  if(AOI == TRUE){
    Rainfalldata <- pivot_longer(Rainfalldata,
                             cols=-c("longitude", "latitude","NAME_1","NAME_2","startingDate", "endDate"),
                             names_to = c("Variable", "Date"),
                             names_sep = "_",
                             values_to = "RAIN")
    Rainfalldata <-unique(dplyr::select(Rainfalldata,-c(Variable,startingDate, endDate)))

    Sraddata <- pivot_longer(Sraddata,
                             cols=-c("longitude", "latitude","NAME_1","NAME_2","startingDate", "endDate"),
                                   names_to = c("Variable", "Date"),
                                   names_sep = "_",
                                   values_to = "SRAD")
    Sraddata <-unique(dplyr::select(Sraddata,-c(Variable,startingDate, endDate)))

    Tmaxdata <- pivot_longer(Tmaxdata,
                             cols=-c("longitude", "latitude","NAME_1","NAME_2","startingDate", "endDate"),
                                   names_to = c("Variable", "Date"),
                                   names_sep = "_",
                                   values_to = "TMAX")
    Tmaxdata <-unique(dplyr::select(Tmaxdata,-c(Variable,startingDate, endDate)))

    Tmindata <- pivot_longer(Tmindata,
                             cols=-c("longitude", "latitude","NAME_1","NAME_2","startingDate", "endDate"),
                                   names_to = c("Variable", "Date"),
                                   names_sep = "_",
                                   values_to = "TMIN")
    Tmindata <-unique(dplyr::select(Tmindata,-c(Variable,startingDate, endDate)))

  }else{
    #We need to confirm the identifier columns in fieldData
    Rainfalldata <- pivot_longer(Rainfalldata,
                             cols=-c("longitude","latitude","startingDate","endDate","yearPi","yearHi","pl_j",
                                     "hv_j","NAME_1","NAME_2"),
                             names_to = c("Variable", "Date"),
                             names_sep = "_",
                             values_to = "RAIN")
     Rainfalldata <-dplyr::select(Rainfalldata,-Variable)

     Sraddata <- pivot_longer(Sraddata,
                              cols=-c("longitude","latitude","startingDate","endDate","yearPi","yearHi","pl_j",
                                      "hv_j","NAME_1","NAME_2"),
                                    names_to = c("Variable", "Date"),
                                    names_sep = "_",
                                    values_to = "SRAD")
     Sraddata <-dplyr::select(Sraddata,-Variable)

     Tmaxdata <- pivot_longer(Tmaxdata,
                              cols=-c("longitude","latitude","startingDate","endDate","yearPi","yearHi","pl_j",
                                      "hv_j","NAME_1","NAME_2"),
                                   names_to = c("Variable", "Date"),
                                   names_sep = "_",
                                   values_to = "TMAX")
    Tmaxdata <-dplyr::select(Tmaxdata,-Variable)

    Tmindata <- pivot_longer(Tmindata,
                             cols=-c("longitude","latitude","startingDate","endDate","yearPi","yearHi","pl_j",
                                     "hv_j","NAME_1","NAME_2"),
                                   names_to = c("Variable", "Date"),
                                   names_sep = "_",
                                   values_to = "TMIN")
    Tmindata <-dplyr::select(Tmindata,-Variable)
  }

  tst <- na.omit(merge(Tmaxdata, merge(Tmindata,merge(Sraddata,Rainfalldata))))
  tst$DATE <- as.POSIXct(tst$Date, format = "%Y-%m-%d", tz = "UTC")
  tst <- dplyr::select(tst,c(DATE,TMAX,TMIN,SRAD,RAIN))
  tst  <- mutate(tst , across(c(TMAX,TMIN,SRAD,RAIN), as.numeric))

  # Avoid TMIN > TMAX
  tst <- tst %>%
    rowwise() %>%
    mutate(
      temp = TMAX,
      temp2 = TMIN,
      TMAX = ifelse(TMIN > TMAX, temp2, TMAX),
      TMIN = ifelse(TMIN > temp, temp, temp2)

    ) %>%
    select(-c(temp,temp2)) %>%
    ungroup()
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
  #elev <- Soil$altitude[which(Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]))]
  #elev <-ifelse(length(elev) >0,
  #              Soil$altitude[which(Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]))],
  #              -99)
#   #Location (NAME_2)
  INS <- toupper(substr(unique(Tmaxdata$NAME_2),start =1, stop =4))
  # Generate new general information table
  general_new <- tibble(
    INSI = INS,
    LAT = as.numeric(coords[i, 2]),
    LONG = as.numeric(coords[i, 1]),
    #ELEV = elev,
    TAV = tav,
    AMP = amp,
    REFHT = 2,
    WNDHT = 2
  )

#   # Add station information
  attr(tst, "GENERAL") <- general_new

  #DSSAT::write_wth(tst, paste0("WHTE",coords[i, 2],"_",coords[i, 1],".WTH"))
  DSSAT::write_wth(tst, paste0("WHTE", formatC(width = 4, (as.integer(i)), flag = "0"), ".WTH"))
  #cat(" Writing weather file")

   ##########################################
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
#' @param pathIn_zone TRUE if the input data (in geo_4cropModel) are organized by zone or province and false if it is just one file 

#' @return weather and soil data in DSSAT format
#' @export
#'
#' @examples readGeo_CM(country = "Kenya",  useCaseName = "KALRO", Crop = "Maize", AOI = TRUE, season=1, Province = "Kiambu")
readGeo_CM_zone <- function(country, useCaseName, Crop, AOI = FALSE, season=1, zone,level2=NA,varietyid,pathIn_zone = T){
  #cat(zone)
  #General input path with all the weather data
  general_pathIn <- paste("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_", useCaseName,"/", Crop, "/result/geo_4cropModel", sep="")
  #define input path based on the organization of the folders by zone and level2 (usually just by zone)
  if (pathIn_zone == T) {
    if(!is.na(level2) & !is.na(zone)){
      pathIn <- paste(general_pathIn,zone,level2, sep = "/")
    }else if(is.na(level2) & !is.na(zone)){
      pathIn <- paste(general_pathIn,zone, sep = "/")
    }else if(!is.na(level2) & is.na(zone)){
      print("You need to define first a zone (administrative level 1) to be able to get data for level 2 (administrative level 2) in datasourcing. Process stopped")
      return(NULL)
    }else{
      pathIn <- general_pathIn
    }
  }else{
    pathIn <- general_pathIn
  }

  if (!dir.exists(pathIn)) {
    stop("You need to provide a path with all the input (weather and soil data) as RDS. Please refer to the documentation. Process stopped")
  }
  
  if(AOI == TRUE){
    Rainfall <- readRDS(paste(pathIn, "/Rainfall_Season_", season, "_PointData_AOI.RDS", sep=""))
    if ("Zone" %in% names(Rainfall)){ names(Rainfall)[names(Rainfall)=="Zone"] <- "NAME_1"}
    if(!is.na(zone)){Rainfall <- Rainfall[Rainfall$NAME_1 == zone, ]}
    if(!is.na(level2)){Rainfall <- Rainfall[Rainfall$NAME_2 == level2, ]}
    
    SolarRadiation <- readRDS(paste(pathIn, "/solarRadiation_Season_", season, "_PointData_AOI.RDS", sep=""))
    if ("Zone" %in% names(SolarRadiation)){ names(SolarRadiation)[names(SolarRadiation)=="Zone"] <- "NAME_1"}
    if(!is.na(zone)){SolarRadiation <- SolarRadiation[SolarRadiation$NAME_1 == zone, ]}
    if(!is.na(level2)){SolarRadiation <- SolarRadiation[SolarRadiation$NAME_2 == level2, ]}
    
    TemperatureMax <- readRDS(paste(pathIn, "/temperatureMax_Season_", season, "_PointData_AOI.RDS", sep=""))
    if ("Zone" %in% names(TemperatureMax)){ names(TemperatureMax)[names(TemperatureMax)=="Zone"] <- "NAME_1"}
    if(!is.na(zone)){TemperatureMax <- TemperatureMax[TemperatureMax$NAME_1 == zone, ]}
    if(!is.na(level2)){TemperatureMax <- TemperatureMax[TemperatureMax$NAME_2 == level2, ]}
    
    TemperatureMin <- readRDS(paste(pathIn, "/temperatureMin_Season_", season, "_PointData_AOI.RDS", sep=""))
    if ("Zone" %in% names(TemperatureMin)){ names(TemperatureMin)[names(TemperatureMin)=="Zone"] <- "NAME_1"}
    if(!is.na(zone)){TemperatureMin <- TemperatureMin[TemperatureMin$NAME_1 == zone, ]}
    if(!is.na(level2)){TemperatureMin <- TemperatureMin[TemperatureMin$NAME_2 == level2, ]}
    
    
    Soil <- readRDS(paste(pathIn,"/SoilDEM_PointData_AOI_profile.RDS", sep=""))
    if ("Zone" %in% names(Soil)){names(Soil)[names(Soil)=="Zone"] <- "NAME_1"}
    if(!is.na(zone)){Soil <- Soil[Soil$NAME_1 == zone, ]}
    if(!is.na(level2)){Soil <- Soil[Soil$NAME_2 == level2, ]}
    
  }else{
    Rainfall <- readRDS(paste(pathIn, "Rainfall_PointData_trial.RDS", sep=""))
    if ("Zone" %in% names(Rainfall)){ names(Rainfall)[names(Rainfall)=="Zone"] <- "NAME_1"}
    if(!is.na(zone)){Rainfall <- Rainfall[Rainfall$NAME_1 == zone, ]}
    if(!is.na(level2)){Rainfall <- Rainfall[Rainfall$NAME_2 == level2, ]}
    
    SolarRadiation <- readRDS(paste(pathIn, "solarRadiation_PointData_trial.RDS", sep=""))
    if ("Zone" %in% names(SolarRadiation)){ names(SolarRadiation)[names(SolarRadiation)=="Zone"] <- "NAME_1"}
    if(!is.na(zone)){SolarRadiation <- SolarRadiation[SolarRadiation$NAME_1 == zone, ]}
    if(!is.na(level2)){SolarRadiation <- SolarRadiation[SolarRadiation$NAME_2 == level2, ]}
    
    TemperatureMax <- readRDS(paste(pathIn, "temperatureMax_PointData_trial.RDS", sep=""))
    if ("Zone" %in% names(TemperatureMax)){ names(TemperatureMax)[names(TemperatureMax)=="Zone"] <- "NAME_1"}
    if(!is.na(zone)){TemperatureMax <- TemperatureMax[TemperatureMax$NAME_1 == zone, ]}
    if(!is.na(level2)){TemperatureMax <- TemperatureMax[TemperatureMax$NAME_2 == level2, ]}
    
    TemperatureMin <- readRDS(paste(pathIn, "temperatureMin_PointData_trial.RDS", sep=""))
    if ("Zone" %in% names(TemperatureMin)){ names(TemperatureMin)[names(TemperatureMin)=="Zone"] <- "NAME_1"}
    if(!is.na(zone)){TemperatureMin <- TemperatureMin[TemperatureMin$NAME_1 == zone, ]}
    if(!is.na(level2)){TemperatureMin <- TemperatureMin[TemperatureMin$NAME_2 == level2, ]}
    
    Soil <- readRDS(paste(pathIn, "SoilDEM_PointData_trial_profile.RDS", sep=""))
    if ("Zone" %in% names(Soil)){names(Soil)[names(Soil)=="Zone"] <- "NAME_1"}
    if(!is.na(zone)){Soil <- Soil[Soil$NAME_1 == zone, ]}
    if(!is.na(level2)){Soil <- Soil[Soil$NAME_2 == level2, ]}
  }
  
  #Modify names created for some of the use cases with different column names
  
  if ("lat" %in% names(Rainfall)){ names(Rainfall)[names(Rainfall)=="lat"] <- "latitude"}
  if ("lon" %in% names(Rainfall)){ names(Rainfall)[names(Rainfall)=="lon"] <- "longitude"}
  if ("country" %in% colnames(Rainfall)) {Rainfall <- subset(Rainfall,select =-country)}
  
  if ("lat" %in% names(TemperatureMax)){ names(TemperatureMax)[names(TemperatureMax)=="lat"] <- "latitude"}
  if ("lon" %in% names(TemperatureMax)){ names(TemperatureMax)[names(TemperatureMax)=="lon"] <- "longitude"}
  if ("country" %in% colnames(TemperatureMax)) {TemperatureMax <- subset(TemperatureMax,select =-country)}
  
  
  if ("lat" %in% names(TemperatureMin)){ names(TemperatureMin)[names(TemperatureMin)=="lat"] <- "latitude"}
  if ("lon" %in% names(TemperatureMin)){ names(TemperatureMin)[names(TemperatureMin)=="lon"] <- "longitude"}
  if ("country" %in% colnames(TemperatureMin)) {TemperatureMin <- subset(TemperatureMin,select =-country)}
  
  if ("lat" %in% names(SolarRadiation)){ names(SolarRadiation)[names(SolarRadiation)=="lat"] <- "latitude"}
  if ("lon" %in% names(SolarRadiation)){ names(SolarRadiation)[names(SolarRadiation)=="lon"] <- "longitude"}
  if ("country" %in% colnames(SolarRadiation)) {SolarRadiation <- subset(SolarRadiation,select =-country)}
  #Soil <- na.omit(Soil) #Avoid removing some points due to missing variables (to check if that would make fail the simulations)
  Soil <- na.omit(Soil) 

  if(AOI == TRUE){
    metaDataWeather <- as.data.frame(Rainfall[,c("longitude", 'latitude', "startingDate", "endDate", "NAME_1", "NAME_2")])
     
  }else{
    metaDataWeather <- as.data.frame(Rainfall[,c("longitude", 'latitude', "startingDate", "endDate", "NAME_1", "NAME_2",
                                                 "yearPi","yearHi","pl_j","hv_j")])
  
  }
  metaData_Soil <-Soil[,c("longitude", "latitude","NAME_1","NAME_2")]

 #Create a general metadata that has unique virtual experiments with unique weather, soil, planting and harvesting date
  metaData <- merge(metaDataWeather,metaData_Soil)


  #Keep all the soil data with rainfall data
  Soil <- merge(unique(metaData[,c("longitude", "latitude","NAME_1","NAME_2")]),Soil)


  #### Keep all the weather data that has soil data ###
  Rainfall <- merge(metaData,Rainfall)
  SolarRadiation <- merge(metaData,SolarRadiation)
  TemperatureMax <- merge(metaData,TemperatureMax)
  TemperatureMin <- merge(metaData,TemperatureMin)
  # RelativeHum <- merge(metaData,RelativeHum)


  # Set working directory to save the results (weather and soil data in DSSAT format)
  if(AOI == TRUE){
    path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", 
                             country, "_",useCaseName, "/", Crop, "/transform/DSSAT/AOI/",varietyid, sep="")
    }
  else{
    path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", 
                             country, "_",useCaseName, "/", Crop, "/transform/DSSAT/fieldData/",varietyid, sep="")
  }
  
  if (!dir.exists(file.path(path.to.extdata))){
    dir.create(file.path(path.to.extdata), recursive = TRUE)
  }

  #Define working directory with template data (soil and weather files in DSSAT format as template)
  path.to.temdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", 
                           country, "_",useCaseName, "/", Crop, "/Landing/DSSAT/", sep="")
  if (!dir.exists(path.to.temdata)){
    print("Directory with template data (soil and weather files in DSSAT) does not exist, please add the template files. Process will stop.")
    dir.create(file.path(path.to.temdata), recursive = TRUE)
    return(NULL)
  }


## Define the unique locations to run the experiments in DSSAT
## when AOI=TRUE it is created weather and soil data by location (unique("longitude", "latitude","NAME_1","NAME_2"))
## when AOI=FALSE (when we have observed field data) it is created weather and soil data by trial 
## (unique(longitude,latitude,"yearPi","yearHi","pl_j","hv_j"))
  
  coords <- metaData
##The following lines are removed because metaData is already a subset by zone and level2 (in case they are defined in the arguments)
  # if(AOI==TRUE){
  #   coords <- coords[(coords$NAME_1 == zone & coords$NAME_2 == level2), ]
  # }else{
  #   coords <- coords[(coords$NAME_1 == zone),]
  # }
  
  if(AOI==TRUE){
    coords <- unique(metaData[,c("longitude", "latitude")])
  }else{
    coords <- metaData
  }

  grid <- as.matrix(coords)
  # Create a list of indices
  indices <- seq_along(grid[,1])
  # 
  # path.to.extdata=path.to.extdata; path.to.temdata=path.to.temdata; Tmaxdata=TemperatureMax; Tmindata=TemperatureMin; Sraddata=SolarRadiation;
  # # # Rainfalldata=Rainfall; RelativeHum=RelativeHum
  
  # Previous way of simulating but less efficient  
  # results <- map(seq_along(grid[,1]), process_grid_element, country=country, path.to.extdata=path.to.extdata,
  #                path.to.temdata=path.to.temdata, Tmaxdata=TemperatureMax, Tmindata=TemperatureMin, Sraddata=SolarRadiation,
  #                Rainfalldata=Rainfall, coords=coords, Soil=Soil, AOI=AOI,varietyid=varietyid,zone=zone, level2=level2) %||% print("Progress:")

  log_file <- paste(path.to.extdata,"progress_log_readGeo_CM.txt",sep='/')
  
  if (file.exists(log_file)) {
    file.remove(log_file)
  }
  
  
  # Set up parallel processing (for more efficient processing)
  plan(multisession, workers = 11)
  
  results <- future_lapply(indices, function(i) {
    message <- paste("Progress experiment:", i, "out of", length(indices),"for variety", varietyid)
    cat(message, "\n", file = log_file, append = TRUE)
    process_grid_element(i, country=country, path.to.extdata=path.to.extdata,
                         path.to.temdata=path.to.temdata, Tmaxdata=TemperatureMax, Tmindata=TemperatureMin,
                         Sraddata=SolarRadiation, Rainfalldata=Rainfall, coords=coords, Soil=Soil,
                         AOI=AOI, varietyid=varietyid, zone=zone, level2=level2)
    message2 <- paste("Finished:", i, "out of", length(indices),"for variety", varietyid)
    cat(message2, "\n", file = log_file, append = TRUE)
  })
  
}
