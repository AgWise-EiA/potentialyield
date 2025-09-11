process_grid_element_experiment <- function(i,path.to.extdata,path.to.temdata,zone,level2=NA,expfile_name,clck,varietyid,ppln,rep1,rep2) {
  
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
  
  #Define the weather data for each location
  apsimx::edit_apsimx(expfile_name, 
                      src.dir = path.to.temdata,
                      wrt.dir = pathOUT,
                      root = c("pd", "Base_one"),
                      node = "Weather", 
                      value = paste0(pathOUT,"/", 'wth_loc_',i,'.met'), 
                      overwrite = TRUE)
  
  #Add the soil profile (ex_profile)
  
  load(paste0(pathOUT,"/my_sol_",i,".RData"))
  
  edit_apsimx_replace_soil_profile(expfile_name, 
                                   src.dir = pathOUT,
                                   wrt.dir = pathOUT,
                                   root = c("pd", "Base_one"), 
                                   soil.profile = ex_profile, 
                                   overwrite = TRUE)
  
  #Modify the number of years of the simulations
  apsimx::edit_apsimx(expfile_name, 
                      src.dir = pathOUT,
                      wrt.dir = pathOUT,
                      root = c("pd", "Base_one"),
                      node = "Clock",
                      parm = c("Start", "End"),
                      value = clck,
                      overwrite = TRUE)
  
  apsimx::edit_apsimx(expfile_name, 
                      src.dir = pathOUT,
                      wrt.dir = pathOUT,
                      root = c("pd", "Base_one"),
                      node = "Manager",
                      manager.child = "SowingRule",
                      parm = "CultivarName", ## This is for cultivar
                      value = varietyid,
                      overwrite = TRUE)
  apsimx::edit_apsimx(expfile_name,
                      src.dir = pathOUT,
                      wrt.dir = pathOUT,
                      root = c("pd", "Base_one"),
                      node = "Manager",
                      manager.child = "SowingRule",
                      parm = "Population", ## This is for population
                      value = ppln,
                      verbose = TRUE, overwrite = TRUE)
  apsimx::edit_apsimx(expfile_name,
                      src.dir = pathOUT,
                      wrt.dir = pathOUT,
                      root = c("pd", "Base_one"),
                      node = "Report",
                      parm = "VariableNames", 
                      value = rep1, 
                      verbose = TRUE, overwrite = TRUE)
  apsimx::edit_apsimx(expfile_name,
                      src.dir = pathOUT,
                      wrt.dir = pathOUT,
                      root = c("pd", "Base_one"),
                      node = "Report",
                      parm = "VariableNames", 
                      value = rep2, 
                      verbose = TRUE, overwrite = TRUE)

}

#wkdir = Working directory where your files will be saved
#cell = The spatial resolution you want e.g 1 degree
#b = Choose the country shapefile you want e.g "ZM" for Zimbabwe
#date = How may years of weather do you want to download e.g c("1985-01-01","2022-01-01")
#crop = The crop in APSIM you want to simulate e.g. "maize.apsimx"
#clck = How many years do you want the simulation to run e.g. c("1985-01-01T00:00:00", "2020-12-31T00:00:00")
#sd = The start date e.g.  "1-jan"
#ed = The end date e.g.  "31-dec"
#variety = The cultivar you want to simulate e.g "A_103"
#rep1 = An additional value to report e.g. "[Maize].Grain.Total.Wt*10 as Yield" ,
#rep2 =An additional value to report e.g. "[Maize].SowingDate"
#' Title
#'
#' @param scfl 
#' @param my_list_clm 
#' @param wkdir 
#' @param crop 
#' @param clck 
#' @param variety 
#' @param ppln 
#' @param rep1 
#' @param rep2 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' 
apsimSpatialFactorial <- function(country,useCaseName,Crop, AOI = FALSE, season=1,zone,level2=NA,pathIn_zone=T,expfile_name,clck,varietyid,ppln,rep1,rep2) {

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
  
  
  
if(AOI == TRUE){
  path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", 
                           country, "_",useCaseName, "/", Crop, "/transform/APSIM/AOI/",varietyid, sep="")
}else{
  path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", 
                           country, "_",useCaseName, "/", Crop, "/transform/APSIM/fieldData/",varietyid, sep="")
}
  
  coords <- metaData

  
  if(AOI==TRUE){
    coords <- unique(metaData[,c("longitude", "latitude")])
  }else{
    coords <- metaData
  }
  
  grid <- as.matrix(coords)
  # Create a list of indices
  indices <- seq_along(grid[,1]) 
  
  log_file <- paste(path.to.extdata,"progress_log_expfile_APSIM.txt",sep='/')
  
  if (file.exists(log_file)) {
    file.remove(log_file)
  }
  
  
  # Set up parallel processing (for more efficient processing)
  num_cores <- availableCores() -3
  plan(multisession, workers = num_cores)
  
  results <- future_lapply(indices, function(i) {
    message <- paste("Progress experiment:", i, "out of", length(indices),"for variety", varietyid)
    cat(message, "\n", file = log_file, append = TRUE)
    process_grid_element_experiment(i,path.to.extdata=path.to.extdata,path.to.temdata=path.to.temdata,
                                    zone=zone,level2=level2,expfile_name=expfile_name,clck=clck,
                                    varietyid=varietyid,ppln=ppln,rep1=rep1,rep2=rep2)
      
    message2 <- paste("Finished:", i, "out of", length(indices),"for variety", varietyid)
    cat(message2, "\n", file = log_file, append = TRUE)
  })

}


