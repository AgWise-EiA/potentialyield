packages_required <- c("tidyverse","furrr","future", "future.apply","apsimx", "jsonlite")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))

# Recursive function to update LL values
fix_AirDry_LL <- function(node) {
  
  # Process Physical nodes
  if (!is.null(node$Children) && node$`$type` == "Models.Soils.Physical, Models") {
    # Find AirDry
    if (!is.null(node$AirDry)) {
      # Loop through children to find SoilCrop nodes
      for (child in node$Children) {
        if (!is.null(child$LL)) {
          # Make all values in AirDry <= LL
          for (i in seq_along(child$LL)) {
            if (node$AirDry[[i]] > child$LL[[i]]) {
              node$AirDry[[i]] <- child$LL[[i]]
            }
          }
        }
      }
    }
  }
  
  # Recurse into children
  if (!is.null(node$Children)) {
    for (i in seq_along(node$Children)) {
      node$Children[[i]] <- fix_AirDry_LL(node$Children[[i]])
    }
  }
  
  return(node)
}


# Function to ensure AirDry <=  LL15
fix_AirDry_LL15 <- function(node, epsilon = 1e-4) {
  
  # Process Physical nodes
  if (!is.null(node$Children) && node$`$type` == "Models.Soils.Physical, Models") {
    
    # Ensure AirDry <= LL15
    if (!is.null(node$AirDry) && !is.null(node$LL15)) {
      for (i in seq_along(node$AirDry)) {
        if (node$AirDry[[i]] > node$LL15[[i]]) {
          node$AirDry[[i]] <- node$LL15[[i]] - epsilon
        }
      }
    }
  }
  
  # Recurse into children
  if (!is.null(node$Children)) {
    for (i in seq_along(node$Children)) {
      node$Children[[i]] <- fix_AirDry_LL15(node$Children[[i]], epsilon)
    }
  }
  
  return(node)
}


# Recursive function to fix SAT values
fix_SAT <- function(node, PD = 2.65, epsilon = 1e-4) {
  
  # Process Physical nodes
  if (!is.null(node$Children) && node$`$type` == "Models.Soils.Physical, Models") {
    
    # Fix SAT <= 1 - BD / PD
    if (!is.null(node$SAT) && !is.null(node$BD)) {
      for (i in seq_along(node$SAT)) {
        max_sat <- 1 - node$BD[[i]] / PD
        if (node$SAT[[i]] > max_sat) {
          node$SAT[[i]] <- max_sat - epsilon
        }
      }
    }
  }
  
  # Recurse into children
  if (!is.null(node$Children)) {
    for (i in seq_along(node$Children)) {
      node$Children[[i]] <- fix_SAT(node$Children[[i]], PD, epsilon)
    }
  }
  
  return(node)
}


# Function to fix start and end dates in APSIM clock
fix_start_end_dates <- function(expfile_name, met_file, out_file = NULL){
  lines <- readLines(met_file)
  
  # Find the header line number
  header_line <- grep("^year", lines, ignore.case = TRUE)
  if(length(header_line) == 0) stop("Could not find weather header in met file.")
  
  # Reader weather data as table
  weather <- read.table(text = lines[(header_line+1):length(lines)], header = FALSE)
  names(weather) <- strsplit(lines[header_line], "\\s+")[[1]]
  
  # Remove units row
  weather <- weather[-1, ]
  row.names(weather) <- NULL
  
  # Format dates to match APSIM format
  weather$date <- as.Date(as.numeric(weather$day) - 1, origin = paste0(as.numeric(weather$year), "-01-01"))
  
  # First and last dates
  start_date <- min(weather$date)
  end_date <- max(weather$date)
  
  # Read .apsimx file
  apsim <- fromJSON(expfile_name, simplifyVector = FALSE)
  
  # Substitute values
  apsim$Children[[2]]$Children[[3]]$Children[[1]]$Start <- paste0(start_date, "T00:00:00")
  apsim$Children[[2]]$Children[[3]]$Children[[1]]$End <- paste0(end_date, "T00:00:00")
  
  # Save corrected .apsimx
  if(is.null(out_file)) {
    out_file <- expfile_name
  }
  
  write_json(apsim, out_file, pretty = TRUE, auto_unbox = TRUE, null = "null")
  
  message("Clock (Start-End) updated to: ", apsim$Children[[2]]$Children[[3]]$Children[[1]]$Start, " → ", apsim$Children[[2]]$Children[[3]]$Children[[1]]$End)
}


# Run the simulation for the entire study area  
#' Title length(my_list_clm)
#'
#' @param my_list_clm 
#' @param extd.dir 
#' @param stn 
#'
#' @return
#' @export
#'
#' @examples

runapsim <-function(i,path.to.extdata,expfile_name,AOI=TRUE){
  setwd(paste(path.to.extdata,paste0('EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/"))

  # Run checks for the APSIM file
  apsim_data <- fromJSON(expfile_name, simplifyVector = FALSE)
  

  fixed_data <- fix_AirDry_LL(apsim_data)
  
  fixed_data <- fix_AirDry_LL15(fixed_data)
  
  fixed_data <- fix_SAT(fixed_data,
                        PD = 2.65,
                        epsilon = 1e-2)

  write_json(fixed_data, 
             expfile_name, 
             pretty = TRUE, 
             auto_unbox = TRUE, 
             digits = NA,
             null = "null")

  # Set Start and End date from meteorological file to APSIM exp file
  fix_start_end_dates(expfile_name = expfile_name, 
                      met_file = paste0("wth_loc_", i,".met"),
                      out_file = NULL)

  # Run APSIM
  harvest_data <- apsimx::apsimx(expfile_name, value = "HarvestReport")
  
  write.csv(harvest_data, paste0("HarvestReport_", i, ".csv"), row.names=FALSE)
  
  gc()
}

apsim.exec <- function(country, useCaseName, Crop, AOI = TRUE,expfile_name,varietyid, zone, level2=NA){  
  
  #Set working directory to save the results
  if(AOI == TRUE){
    path.to.extdata_ini <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/APSIM/AOI/",varietyid, sep="")
  }else{
    path.to.extdata_ini <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/APSIM/fieldData/",varietyid, sep="")
  }
  
  
  #define working path or path to run the model
  # path.to.extdata in this script is not the same as in script 2
  if(!is.na(level2) & !is.na(zone)){
    path.to.extdata <- paste(path.to.extdata_ini,zone,level2, sep = "/")
  }else if(is.na(level2) & !is.na(zone)){
    path.to.extdata <- paste(path.to.extdata_ini,zone, sep = "/")
  }else if(!is.na(level2) & is.na(zone)){
    print("You need to define first a zone (administrative level 1) to be able to run the model for level 2 (administrative level 2). Process stopped")
    return(NULL)
  }else{
    path.to.extdata <- path.to.extdata_ini
  }
  if (!dir.exists(file.path(path.to.extdata))){
    print("You need to create the input files (weather, soil and experimental data) before running the model. Process stopped")
    return(NULL)
  }
  
  setwd(path.to.extdata)
  
  folders <- list.dirs(".", full.names = FALSE, recursive = TRUE)
  folders <- grep(folders, pattern = ".ipynb", value = TRUE, invert = TRUE, fixed = TRUE)
  matching_folders <- folders[grepl("EXTE", folders, ignore.case = TRUE)]
  
  
  # Create a list of indices
  indices <- seq_along(matching_folders)
  
  # Create a log file to see the progress of the simulations
  log_file <- paste(path.to.extdata,"progress_log_run.txt",sep='/')
  if (file.exists(log_file)) {
    file.remove(log_file)
  }
  
  num_cores <- availableCores() -3
  plan(multisession, workers = num_cores)
  results <- future_lapply(indices, function(i) {
    message <- paste("Progress:", i, "out of", length(indices))
    cat(message, "\n", file = log_file, append = TRUE)
    
    result <- runapsim(i, path.to.extdata=path.to.extdata,expfile_name=expfile_name,AOI=AOI)
    
    message2 <- paste("Finished:", i, "out of", length(indices))
    cat(message2, "\n", file = log_file, append = TRUE)
    
    return(result)
  })
  plan(sequential)
  rm(list = ls())
  gc()
  
}





# my_list_sim<- function(crop, my_list_clm, extd.dir, stn, my_list_soil){
# 
#   cores<- detectCores()
#   myCluster <- makeCluster(cores -2, # number of cores to use
#                            type = "PSOCK") # type of cluster
#   registerDoParallel(myCluster)
#   my_list_sims<- foreach (i =1:length(my_list_clm)) %dopar% {  
#     setwd(paste0(extd.dir, '/', i)) 
#     # setwd(paste0(extd.dir, '/', i))  
#     tryCatch(apsimx::apsimx(crop, value = "HarvestReport"), error=function(err) NA)
#   }
#   newlist<- foreach (i =1:length(my_list_clm)) %dopar% { 
#     if(is.na(my_list_sims[i]) == TRUE) {
#       setwd(paste0(extd.dir, '/', i)) 
#       #tryCatch({my_list_soil$soil$SAT <-c(0.521, 0.521, 0.497, 0.488, 0.478, 0.440)}, error=function(e) {NA})
#       my_list_soil[[i]]$SoilName_1$crops <- c("Rice","Wheat","Teff","Sugarcane","Maize","Soybean","OilPalm","Cassava")
#       apsimx::edit_apsimx_replace_soil_profile(crop, root = c("pd", "Base_one"), soil.profile = my_list_soil[[i]]$SoilName_1, overwrite = TRUE) 
#       my_list_sims[[i]]<-tryCatch(apsimx::apsimx(crop, value = "HarvestReport"), error=function(err) NA)
#     }
#     else  my_list_sims[[i]]
#   }
#   
#    return(newlist)
# }





