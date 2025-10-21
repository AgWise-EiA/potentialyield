packages_required <- c("arrow", "tidyverse","furrr","future", "future.apply","apsimx", "jsonlite")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))


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

runapsim <-function(i,path.to.zone,expfile_name,AOI=TRUE){
  setwd(paste(path.to.zone, paste0('EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/"))

  # Run APSIM
  harvest_data <- apsimx::apsimx(expfile_name, value = "HarvestReport")
  
  write_parquet(harvest_data, paste0("HarvestReport_", i, ".parquet"))
  
  gc()
}

apsim.exec <- function(country, useCaseName, Crop, AOI = TRUE,expfile_name,varietyid, zone, level2=NA){  
  
  #Set working directory to save the results
  zone <- gsub(" ", "_", zone)
  if(AOI == TRUE){
    path.to.varietyid <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/APSIM/AOI/",varietyid, sep="")
  }else{
    path.to.varietyid <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/APSIM/fieldData/",varietyid, sep="")
  }
  
  
  # define working path or path to run the model
  if(!is.na(level2) & !is.na(zone)){
    path.to.zone <- paste(path.to.varietyid, zone, level2, sep = "/")
  }else if(is.na(level2) & !is.na(zone)){
    path.to.zone <- paste(path.to.varietyid, zone, sep = "/")
  }else if(!is.na(level2) & is.na(zone)){
    print("You need to define first a zone (administrative level 1) to be able to run the model for level 2 (administrative level 2). Process stopped")
    return(NULL)
  }else{
    path.to.zone <- path.to.varietyid
  }
  if (!dir.exists(file.path(path.to.zone))){
    print("You need to create the input files (weather, soil and experimental data) before running the model. Process stopped")
    return(NULL)
  }
  
  setwd(path.to.zone)
  
  # APSIMX does not allow for white spaces in paths
  safe.path.to.zone <- gsub(" ", "_", path.to.zone)
  renamed_path <- FALSE
  if (!dir.exists(safe.path.to.zone) && dir.exists(path.to.zone)) {
    # Remove symbolic link if it exists
    target <- Sys.readlink(safe.path.to.zone)
    if (!is.na(target) & nzchar(target)){
      unlink(safe.path.to.zone, recursive=TRUE)
    }
    file.rename(path.to.zone, safe.path.to.zone)
    setwd(safe.path.to.zone)
    renamed_path <- TRUE
  }


  folders <- list.dirs(".", full.names = FALSE, recursive = FALSE)
  folders <- grep(folders, pattern = ".ipynb", value = TRUE, invert = TRUE, fixed = TRUE)
  matching_folders <- folders[grepl("EXTE", folders, ignore.case = TRUE)]
  
  
  # Create a list of indices
  indices <- seq_along(matching_folders)
  
  # Create a log file to see the progress of the simulations
  log_file <- paste(safe.path.to.zone,"progress_log_run.txt",sep='/')
  if (file.exists(log_file)) {
    file.remove(log_file)
  }
  
  num_cores <- availableCores() -3
  plan(multisession, workers = num_cores)
  # Removed storing results because they are being saved to a csv file
  # results <- future_lapply(indices, function(i) {
  future_lapply(indices, function(i) {
    message <- paste("Progress:", i, "out of", length(indices))
    cat(message, "\n", file = log_file, append = TRUE)
    
    # result <- runapsim(i, path.to.zone=path.to.zone,expfile_name=expfile_name,AOI=AOI)
    runapsim(i, path.to.zone=safe.path.to.zone,expfile_name=expfile_name,AOI=AOI)
    
    message2 <- paste("Finished:", i, "out of", length(indices))
    cat(message2, "\n", file = log_file, append = TRUE)
    
    # return(result)
  })
  plan(sequential)
  if (renamed_path){
    file.rename(safe.path.to.zone, path.to.zone)
  }
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

