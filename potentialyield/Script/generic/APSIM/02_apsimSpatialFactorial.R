packages_required <- c("tidyverse","sf","furrr","future", "future.apply","parallel","sp","apsimx")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))

#' fix_start_end_dates
#'
#' Adjusts the APSIM Clock start and end dates based on a user-defined range (`clck`)
#' and a weather file (`met_file`). The function sets the simulation period to the
#' most limiting dates: the later of the requested start and weather start, and
#' the earlier of the requested end and weather end.
#'
#' @param expfile_name Character. Path to the APSIMX experiment file (.apsimx).
#' @param clck Character vector of length 2. User-defined start and end dates 
#'             in APSIMX format: c("YYYY-MM-DDT00:00:00", "YYYY-MM-DDT00:00:00").
#' @param met_file Character. Path to the weather file (.met) used for the experiment.
#' @param pathOUT Character. Directory where the APSIMX file resides and will be written.
#'
#' @return Updates the Clock node in the APSIMX experiment file with the computed
#'         start and end dates. Prints a message confirming the update.
#'
#' @details
#' - Reads the weather file and determines its start and end dates.
#' - Computes the most limiting start and end dates using the intersection of
#'   `clck` and the weather file range.
#' - Uses `apsimx::edit_apsimx()` to update the Clock node.
#' - Ensures the simulation period does not exceed the weather file boundaries.
#'
#' @examples
#' clck <- c("1981-01-01T00:00:00", "2020-12-31T00:00:00")
#' fix_start_end_dates("Base_one.apsimx", clck, "weather.met", pathOUT = "apsim_project")

fix_start_end_dates <- function(expfile_name, clck, met_file, pathOUT) {
  # Validate clck
  if(length(clck) != 2) stop("clck must be a vector of length 2: c(Start, End)")
  
  # Read weather file
  lines <- readLines(met_file)
  header_line <- grep("^year", lines, ignore.case = TRUE)
  if(length(header_line) == 0) stop("Could not find weather header in met file.")
  
  weather <- read.table(text = lines[(header_line+1):length(lines)], header = FALSE)
  names(weather) <- strsplit(lines[header_line], "\\s+")[[1]]
  weather <- weather[-1, ]
  row.names(weather) <- NULL
  
  
  # Remove units row if present
  if(all(grepl("[a-zA-Z]", weather[1, ]))) weather <- weather[-1, ]
  
  # Compute weather start and end dates
  weather$date <- as.Date(as.numeric(weather$day) - 1, origin = paste0(as.numeric(weather$year), "-01-01"))
  weather_start <- paste0(min(weather$date), "T00:00:00")
  weather_end   <- paste0(max(weather$date), "T00:00:00")
  
  # Determine most limiting start and end
  final_start <- max(clck[1], weather_start)
  final_end   <- min(clck[2], weather_end)
  final_clck  <- c(final_start, final_end)
  
  # Update APSIM Clock
  apsimx::edit_apsimx(
    expfile_name,
    src.dir = pathOUT,
    wrt.dir = pathOUT,
    root = c("pd", "Base_one"),  # adjust to your experiment structure
    node = "Clock",
    parm = c("Start", "End"),
    value = final_clck,
    overwrite = TRUE
  )
  
  message("Clock (Start-End) updated to: ", final_clck[1], " → ", final_clck[2])
}


#' adjust_soil_profile
#'
#' Ensures APSIMX soil assumptions are satisfied by adjusting either soil water
#' parameters (LL15, DUL, SAT) or crop lower limits (LLs).
#'
#' @param soil_df Data frame with soil profile (AirDry, LL15, DUL, SAT, BD, Maize.LL, Soybean.LL, Wheat.LL)
#' @param modify "soil" to adjust soil parameters to fit crops, "crop" to adjust crops to fit soil
#' @return Modified soil_df satisfying: AirDry < LL < DUL < SAT, SAT < 1-BD/PD, Crop LLs within AirDry-DUL
#' @details Prints messages for any adjustments made.
#' @examples
#' fixed_soil <- adjust_soil_profile(ex_profile$soil, modify = "soil")
#' fixed_crops <- adjust_soil_profile(ex_profile$soil, modify = "crop")

adjust_soil_profile <- function(soil_df, modify = c("crop", "soil"), epsilon = 0.0001) {
  modify <- match.arg(modify)
  
  PD <- 2.65  # particle density (g/cm3)
  max_sat <- 1 - soil_df$BD / PD
  
  # Loop through layers
  for(i in 1:nrow(soil_df)) {
    
    airdry <- soil_df$AirDry[i]
    ll <- soil_df$LL15[i]
    dul <- soil_df$DUL[i]
    sat <- soil_df$SAT[i]
    
    crops <- c("Maize.LL", "Soybean.LL", "Wheat.LL", "Barley.LL", "Sorghum.LL",
               "Rice.LL")
    
    crops <- crops[crops %in% names(soil_df)]
    
    crop_lls <- sapply(crops, function(c) soil_df[[c]][i])
    min_crop_ll <- min(crop_lls)
    
    # Identify violations
    violate_airdry <- airdry >= ll
    violate_ll_dul <- ll >= dul
    violate_dul_sat <- dul >= sat
    violate_sat_max <- sat > max_sat[i]
    violate_crop <- any(crop_lls <= airdry | crop_lls >= dul | crop_lls <= ll)
    
    any_violation <- violate_airdry || violate_ll_dul || violate_dul_sat || violate_sat_max || violate_crop
    
    if(!any_violation) next
    
    if(modify == "soil") {
      # Adjust SAT < max_sat
      if(sat > max_sat[i]) {
        message(sprintf("Layer %d: SAT adjusted from %.3f to %.3f (max allowed %.3f)", 
                        i, sat, max_sat[i], max_sat[i]))
        sat <- max_sat[i]
        soil_df$SAT[i] <- sat
      }
      
      # Adjust DUL < SAT
      if(dul >= sat) {
        dul <- sat - epsilon
        message(sprintf("Layer %d: DUL adjusted to %.3f to be < SAT %.3f", i, dul, sat))
        soil_df$DUL[i] <- dul
      }
      
      # Adjust DUL: > LL15
      lower_bound <- max(ll, min_crop_ll)
      if(dul <= lower_bound) {
        dul <- (lower_bound + sat)/2  # Adjusted in between SAT and a valid LL
        message(sprintf("Layer %d: DUL adjusted from %.3f to %.3f to be > LL15 %.3f", i, soil_df$DUL[i], dul, ll))
        soil_df$DUL[i] <- dul
      }
      
      # Adjust LL15: < min(DUL, min_crop_LL)
      upper_limit <- min(dul, min_crop_ll)
      if(ll >= upper_limit) {
        ll <- upper_limit - epsilon
        message(sprintf("Layer %d: LL15 adjusted to %.3f to be < min(DUL %.3f, min crop LL %.3f)", 
                        i, ll, dul, min_crop_ll))
        soil_df$LL15[i] <- ll
      }
      
      # Adjust AirDry: > min(LL15, min_crop_LL)
      min_limit <- min(ll, min_crop_ll)
      if(airdry >= min_limit) {
        airdry <- min_limit - epsilon
        message(sprintf("Layer %d: AirDry adjusted from %.3f to %.3f to be < LL15 %.3f", 
                        i, soil_df$AirDry[i], airdry, ll))
        soil_df$AirDry[i] <- airdry
      }
      
    } else if(modify == "crop") {
      # Adjust each crop LL to be > AirDry, < DUL
      for(crop in crops) {
        crop_ll <- soil_df[[crop]][i]
        new_crop_ll <- crop_ll
        if(crop_ll <= airdry) {
          new_crop_ll <- airdry + epsilon
          message(sprintf("Layer %d: %s adjusted from %.3f to %.3f (AirDry %.3f)", 
                          i, crop, crop_ll, new_crop_ll, airdry))
        }
        if(crop_ll >= dul) {
          new_crop_ll <- dul - epsilon
          message(sprintf("Layer %d: %s adjusted to %.3f to be < DUL %.3f", 
                          i, crop, new_crop_ll, dul))
        }
        soil_df[[crop]][i] <- new_crop_ll
      }
      
      # Adjust SAT if needed
      if(sat > max_sat[i]) {
        message(sprintf("Layer %d: SAT adjusted from %.3f to %.3f (max allowed %.3f)", 
                        i, sat, max_sat[i], max_sat[i]))
        soil_df$SAT[i] <- max_sat[i]
      }
    }
  }
  
  return(soil_df)
}



process_grid_element_experiment <- function(i,path.to.extdata,path.to.temdata,zone,level2=NA,expfile_name,clck,varietyid,rep,fix_crop_or_soil_parm) {
  
  
  if(!is.na(level2) & !is.na(zone)){
    pathOUT1 <- paste(path.to.extdata,paste0(zone,'/', level2), sep='/')
  }else if(is.na(level2) & !is.na(zone)){
    pathOUT1 <- paste(path.to.extdata, paste0(zone), sep='/')
  }else if(!is.na(level2) & is.na(zone)){
    print("You need to define first a zone (administrative level 1) to be able to get data for level 2 (administrative level 2) in the creation of soil and weather files. Process will stop")
    return(NULL)
  }else{
    pathOUT1 <- path.to.extdata
  }
  
  # Safe path, since APSIM does not take white spaces in their paths
  # Create a symbolic link to directories with provinces that have white spaces in their names
  if (grepl(" ", pathOUT1)) {
    safe_path <- gsub(" ", "_", pathOUT1)   # replace spaces with underscores
    if (!file.exists(safe_path)) {
      system2("ln", args = c("-s", shQuote(pathOUT1), shQuote(safe_path)))
    }
    pathOUT1 <- safe_path
  }
  
  if(!is.na(level2) & !is.na(zone)){
    pathOUT <- paste(pathOUT1, paste0('EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/")
  }else if(is.na(level2) & !is.na(zone)){
    pathOUT <- paste(pathOUT1, paste0('EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/")
  }else if(!is.na(level2) & is.na(zone)){
    print("You need to define first a zone (administrative level 1) to be able to get data for level 2 (administrative level 2) in the creation of soil and weather files. Process will stop")
    return(NULL)
  }else{
    pathOUT <- paste(pathOUT1, paste0('EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/")
  }
  if (!dir.exists(file.path(pathOUT))){
    dir.create(file.path(pathOUT), recursive = TRUE)
  }
  
  setwd(pathOUT)
  
  met_file <- paste0(pathOUT,"/", 'wth_loc_',i,'.met')
  #Define the weather data for each location
  apsimx::edit_apsimx(expfile_name, 
                      src.dir = path.to.temdata,
                      wrt.dir = pathOUT,
                      root = c("pd", "Base_one"),
                      node = "Weather", 
                      value = met_file, 
                      overwrite = TRUE)
  
  #Modify the number of years of the simulations based on available data and selected time period
  fix_start_end_dates(expfile_name = expfile_name, 
                      clck = clck, 
                      met_file = met_file, 
                      pathOUT = pathOUT)
  
  #Add the soil profile (ex_profile)
  
  load(paste0(pathOUT,"/my_sol_",i,".RData"))
  
  modified_soil <- adjust_soil_profile(soil_df = ex_profile$soil,
                                       modify = fix_crop_or_soil_parm)
  
  ex_profile$soil <- modified_soil
  
  edit_apsimx_replace_soil_profile(expfile_name, 
                                   src.dir = pathOUT,
                                   wrt.dir = pathOUT,
                                   root = c("pd", "Base_one"), 
                                   soil.profile = ex_profile, 
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
  # This has been dropped
  # apsimx::edit_apsimx(expfile_name,
  #                     src.dir = pathOUT,
  #                     wrt.dir = pathOUT,
  #                     root = c("pd", "Base_one"),
  #                     node = "Manager",
  #                     manager.child = "SowingRule",
  #                     parm = "Population", ## This is for population
  #                     value = ppln,
  #                     verbose = TRUE, overwrite = TRUE)
  for (report in rep){
  apsimx::edit_apsimx(expfile_name,
                      src.dir = pathOUT,
                      wrt.dir = pathOUT,
                      root = c("pd", "Base_one"),
                      node = "Report",
                      parm = "VariableNames", 
                      value = rep, 
                      verbose = TRUE, overwrite = TRUE)
    }

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
apsimSpatialFactorial <- function(country,useCaseName,Crop, AOI = FALSE, season=1,zone,level2=NA,pathIn_zone=T,expfile_name,clck,varietyid,rep,fix_crop_or_soil_parm) {

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
  
  path.to.temdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", 
                           country, "_",useCaseName, "/", Crop, "/Landing/APSIM/", sep="")
  
  # Set up parallel processing (for more efficient processing)
  num_cores <- availableCores() -3
  plan(multisession, workers = num_cores)
  
  results <- future_lapply(indices, function(i) {
    message <- paste("Progress experiment:", i, "out of", length(indices),"for variety", varietyid)
    cat(message, "\n", file = log_file, append = TRUE)
    process_grid_element_experiment(i,path.to.extdata=path.to.extdata,path.to.temdata=path.to.temdata,
                                    zone=zone,level2=level2,expfile_name=expfile_name,clck=clck,
                                    varietyid=varietyid,rep=rep,fix_crop_or_soil_parm=fix_crop_or_soil_parm)
      
    message2 <- paste("Finished:", i, "out of", length(indices),"for variety", varietyid)
    cat(message2, "\n", file = log_file, append = TRUE)
  })

}


