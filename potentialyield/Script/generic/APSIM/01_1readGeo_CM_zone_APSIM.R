# Create weather and soil files in DSSAT format

# Introduction: 
# This script allows the creation of weather and soil files up to administrative level 2
# Authors : P.Moreno, A. Sila, S. Mkuhlani, E.Bendito Garcia 
# Credentials : EiA, 2024
# Last modified June 28, 2024 

#################################################################################################################
## sourcing required packages                                                                                  ##
#################################################################################################################
options(future.globals.maxSize = 6* 1024^3)

packages_required <- c("chirps", "tidyverse","sf","furrr","future", "future.apply","parallel","sp","apsimx")

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

depth_names = function(var_name, depths){
  list_depthnames = list("5"= "0-5cm","15" = "5-15cm","30" = "15-30cm","60" = "30-60cm","100" = "60-100cm","200" = "100-200cm")
  return (sapply(depths, function(d) paste0(var_name, "_", list_depthnames[as.character(d)])))
}
                 
process_grid_element <- function(i,country,path.to.extdata,path.to.temdata,Tmaxdata,Tmindata,Sraddata,Rainfalldata,coords,Soil,AOI,varietyid,zone,level2=NA,Depth = c(5,15,30,60,100,200) ) {

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
                           values_to = "rain")
  Rainfalldata <-unique(dplyr::select(Rainfalldata,-c(Variable,startingDate, endDate)))

  Sraddata <- pivot_longer(Sraddata,
                           cols=-c("longitude", "latitude","NAME_1","NAME_2","startingDate", "endDate"),
                                 names_to = c("Variable", "Date"),
                                 names_sep = "_",
                                 values_to = "radn")
  Sraddata <-unique(dplyr::select(Sraddata,-c(Variable,startingDate, endDate)))

  Tmaxdata <- pivot_longer(Tmaxdata,
                           cols=-c("longitude", "latitude","NAME_1","NAME_2","startingDate", "endDate"),
                                 names_to = c("Variable", "Date"),
                                 names_sep = "_",
                                 values_to = "maxt")
  Tmaxdata <-unique(dplyr::select(Tmaxdata,-c(Variable,startingDate, endDate)))

  Tmindata <- pivot_longer(Tmindata,
                           cols=-c("longitude", "latitude","NAME_1","NAME_2","startingDate", "endDate"),
                                 names_to = c("Variable", "Date"),
                                 names_sep = "_",
                                 values_to = "mint")
  Tmindata <-unique(dplyr::select(Tmindata,-c(Variable,startingDate, endDate)))

}else{
  #We need to confirm the identifier columns in fieldData
  Rainfalldata <- pivot_longer(Rainfalldata,
                           cols=-c("longitude","latitude","startingDate","endDate","yearPi","yearHi","pl_j",
                                   "hv_j","NAME_1","NAME_2"),
                           names_to = c("Variable", "Date"),
                           names_sep = "_",
                           values_to = "rain")
   Rainfalldata <-dplyr::select(Rainfalldata,-Variable)

   Sraddata <- pivot_longer(Sraddata,
                            cols=-c("longitude","latitude","startingDate","endDate","yearPi","yearHi","pl_j",
                                    "hv_j","NAME_1","NAME_2"),
                                  names_to = c("Variable", "Date"),
                                  names_sep = "_",
                                  values_to = "radn")
   Sraddata <-dplyr::select(Sraddata,-Variable)

   Tmaxdata <- pivot_longer(Tmaxdata,
                            cols=-c("longitude","latitude","startingDate","endDate","yearPi","yearHi","pl_j",
                                    "hv_j","NAME_1","NAME_2"),
                                 names_to = c("Variable", "Date"),
                                 names_sep = "_",
                                 values_to = "maxt")
  Tmaxdata <-dplyr::select(Tmaxdata,-Variable)

  Tmindata <- pivot_longer(Tmindata,
                           cols=-c("longitude","latitude","startingDate","endDate","yearPi","yearHi","pl_j",
                                   "hv_j","NAME_1","NAME_2"),
                                 names_to = c("Variable", "Date"),
                                 names_sep = "_",
                                 values_to = "mint")
  Tmindata <-dplyr::select(Tmindata,-Variable)
}

tst <- na.omit(merge(Tmaxdata, merge(Tmindata,merge(Sraddata,Rainfalldata))))
tst$Date <- as.POSIXct(tst$Date, format = "%Y-%m-%d", tz = "UTC")
tst <- dplyr::select(tst,c(Date,rain,maxt,mint,radn))
tst  <- mutate(tst , across(c(rain,maxt,mint,radn), as.numeric))


# Avoid TMIN > TMAX
tst <- tst %>%
  rowwise() %>%
  mutate(
    temp = maxt,
    temp2 = mint,
    maxt = ifelse(mint > maxt, temp2, maxt),
    mint = ifelse(mint > temp, temp, temp2),
    day = lubridate::yday(Date),
    year=format(Date,"%Y")

  ) %>%
  ungroup()

amp <- tst %>%
  # Extract month from DATE column
  mutate(month = lubridate::month(as.Date(tst$Date,format = "%y%j"))) %>%
  # Group data by month
  group_by(month) %>%
  # Calculate monthly means
  dplyr::summarise(monthly_avg = mean((maxt+mint)/2,na.rm=T)) %>%
  # Calculate AMP as half the difference between minimum and
  #     maximum monthly temperature
  dplyr::summarise(AMP = (max(monthly_avg, na.rm=T)-min(monthly_avg,na.rm=T))/2)

tav <- tst %>%
  dplyr::summarise(TAV=mean((maxt+mint)/2,na.rm=T))


tst <- tst %>%
  select(c(year,day,rain,maxt,mint,radn))

location <- toupper(substr(unique(Tmaxdata$NAME_2),start =1, stop =4))
units <- c("()", "()","(mm)","(oC)", "(oC)", "(MJ/m2/day)")
comments <- paste("!data from various areas. retrieved: ", Sys.time())



  attr(tst, "filename") <- paste0(location,".met")
  attr(tst, "site") <- paste("site =", location)
  attr(tst, "latitude") <- paste("latitude =", coords[i, 2])
  attr(tst, "longitude") <- paste("longitude =",coords[i, 1])
  attr(tst, "tav") <- paste("tav =",tav)
  attr(tst, "amp") <- paste("amp =",amp)
  attr(tst, "colnames") <- names(tst)
  attr(tst, "units") <- units
  attr(tst, "comments") <- comments
  ## No constants
  class(tst) <- c("met", "data.frame")
  #tst<- amp_apsim_met(tst)




  # apsimx::write_apsim_met(my_list_clm[[i]], wrt.dir = "D:/APSIM/", filename = paste0('wth_loc_',i,'.met'))}
  apsimx::write_apsim_met(tst, wrt.dir = pathOUT, filename = paste0('wth_loc_',i,'.met'))
  #cat(" Writing weather file")

 #   ##########################################
 #   # Get soil ISRIC data from server
 #   #Depth<-c(5,15,30,60,100,200)
    LL15 <-as.numeric(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]), depth_names("PWP", Depth)])
    DUL  <-as.numeric(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),depth_names("FC", Depth)])
    SAT  <-as.numeric(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),depth_names("SWS", Depth)])
    SKS  <-as.numeric(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),depth_names("KS", Depth)])/10
    SSS  <-round(as.numeric(SKS), digits = 1)
    BDM  <- as.numeric(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),depth_names("bdod", Depth)])
    LOC  <- as.numeric((Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),depth_names("soc", Depth)])/10)
    LCL  <- as.numeric(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),depth_names("clay", Depth)])
    LSI  <- as.numeric(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),depth_names("silt", Depth)])
    LNI  <- c(as.numeric((Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),depth_names("nitrogen", Depth)])/10))
    LHW  <- as.numeric(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),depth_names("phh2o", Depth)])
    #LDR <- as.numeric(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),c("LDR")])
    CEC <- as.numeric(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),depth_names("cec", Depth)])



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


   #SLU <- slu1(Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),c("clay_0-5cm")],Soil[Soil$longitude==as.numeric(coords[i, 1]) & Soil$latitude==as.numeric(coords[i, 2]),c("sand_0-5cm")])

   #Soil root growth factor. Based on formula from DSSAT. Maybe not the best option for soils with duripan or other root growth limitations
   # layer_center <- c(Depth[1]/2, (Depth[-1] - Depth[-length(Depth)]) / 2 + Depth[-length(Depth)])
   # RGF = ifelse(Depth<=15, 1,1 * exp(-0.02 * layer_center))
 # 
   # ex_profile <- NULL
   # retries <- 50
   # delay <- 2
   # for (attempt in seq_len(retries)) {
   #   tryCatch({
   #     ex_profile <- apsimx::get_isric_soil_profile(lonlat = c(as.numeric(coords[i, 1]), as.numeric(coords[i, 2])))  # Attempt to fetch data
   #     return(ex_profile)
   #   }, error = function(e) {
   #     if (attempt < retries) {
   #       Sys.sleep(delay * attempt)  # Exponential backoff
   #       message(sprintf("Retrying (attempt %d) for location %i, lon: %f, lat: %f", attempt, i,coords[i, 1], coords[i, 2]))
   #     } else {
   #       message(sprintf("Failed after %d attempts for location %i,lon: %f, lat: %f. Error: %s", retries,i, coords[i, 1], coords[i, 2], e$message))
   #       ex_profile <- apsimx::get_isric_soil_profile(lonlat = c(as.numeric(coords[i-1, 1]), as.numeric(coords[i-1, 2])))  # Attempt to fetch data from previous point
   #       return(ex_profile)
   #   }
   #     })
   # }
   # 
   #Define working directory with template data (soil and weather files in APSIM format as template)
   path.to.temdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", 
                            country, "_",useCaseName, "/", Crop, "/Landing/APSIM/", sep="")
   if (!dir.exists(path.to.temdata)){
     print("Directory with template data (soil and weather files in APSIM) does not exist, please add the template files. Process will stop.")
     dir.create(file.path(path.to.temdata), recursive = TRUE)
     return(NULL)
   }
   load(paste(path.to.temdata, "my_sol.RData", sep="/"))

 # Replace by internal values estimated
    ex_profile$soil$LL15 <- LL15
    ex_profile$soil$DUL <- DUL
    ex_profile$soil$SAT <- min(SAT,0.697) #To avoid errors running the model
    ex_profile$soil$AirDry <- LL15 - 0.02
    ex_profile$soil$KS <- SSS
    ex_profile$soil$BD <-BDM
    ex_profile$soil$Carbon <-LOC
    ex_profile$soil$ParticleSizeClay<- LCL
    ex_profile$soil$ParticleSizeSilt<- LSI
    ex_profile$soil$Nitrogen <- LNI*100
    ex_profile$soil$PH <- LHW
    ex_profile$soil$CEC <- CEC #To check the units
    ex_profile$soilwat$Salb <- ALB
    ex_profile$soilwat$CN2Bare <- LRO
 

    save(ex_profile, file=paste0(pathOUT,"/my_sol_",i,".RData"))
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
#' @param Depth list of soil depths information 
                 
#' @return weather and soil data in DSSAT format
#' @export
#'
#' @examples readGeo_CM(country = "Kenya",  useCaseName = "KALRO", Crop = "Maize", AOI = TRUE, season=1, Province = "Kiambu")
readGeo_CM_zone_APSIM <- function(country, useCaseName, Crop, AOI = FALSE, season=1, zone,level2=NA,varietyid,pathIn_zone = T, Depth = c(5,15,30,60,100,200)){
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
  if ("lat" %in% names(Soil)){ names(Soil)[names(Soil)=="lat"] <- "latitude"}
  if ("lon" %in% names(Soil)){ names(Soil)[names(Soil)=="lon"] <- "longitude"}
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


  # Set working directory to save the results (weather and soil data in APSIM format)
  if(AOI == TRUE){
    path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", 
                             country, "_",useCaseName, "/", Crop, "/transform/APSIM/AOI/",varietyid, sep="")
    }else{
    path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", 
                             country, "_",useCaseName, "/", Crop, "/transform/APSIM/fieldData/",varietyid, sep="")
  }
  
  if (!dir.exists(file.path(path.to.extdata))){
    dir.create(file.path(path.to.extdata), recursive = TRUE)
  }

  # #Define working directory with template data (soil and weather files in DSSAT format as template)
  # path.to.temdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", 
  #                          country, "_",useCaseName, "/", Crop, "/Landing/DSSAT/", sep="")
  # if (!dir.exists(path.to.temdata)){
  #   print("Directory with template data (soil and weather files in DSSAT) does not exist, please add the template files. Process will stop.")
  #   dir.create(file.path(path.to.temdata), recursive = TRUE)
  #   return(NULL)
  # }


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

  log_file <- paste(path.to.extdata,"progress_log_readGeo_CM_APSIM.txt",sep='/')
  
  if (file.exists(log_file)) {
    file.remove(log_file)
  }
  
  
  # Set up parallel processing (for more efficient processing)
  num_cores <- availableCores() -3
  plan(multisession, workers = num_cores)
  
  results <- future_lapply(indices, function(i) {
    message <- paste("Progress experiment:", i, "out of", length(indices),"for variety", varietyid)
    cat(message, "\n", file = log_file, append = TRUE)
    process_grid_element(i, country=country, path.to.extdata=path.to.extdata,
                         path.to.temdata=path.to.temdata, Tmaxdata=TemperatureMax, Tmindata=TemperatureMin,
                         Sraddata=SolarRadiation, Rainfalldata=Rainfall, coords=coords, Soil=Soil,
                         AOI=AOI, varietyid=varietyid, zone=zone, level2=level2, Depth = Depth)
    message2 <- paste("Finished:", i, "out of", length(indices),"for variety", varietyid)
    cat(message2, "\n", file = log_file, append = TRUE)
  })
  
}
