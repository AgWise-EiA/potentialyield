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
  

  # Run APSIM
  apsimx::apsimx(expfile_name, value = "HarvestReport")

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





my_list_sim<- function(crop, my_list_clm, extd.dir, stn, my_list_soil){

  cores<- detectCores()
  myCluster <- makeCluster(cores -2, # number of cores to use
                           type = "PSOCK") # type of cluster
  registerDoParallel(myCluster)
  my_list_sims<- foreach (i =1:length(my_list_clm)) %dopar% {  
    setwd(paste0(extd.dir, '/', i)) 
    # setwd(paste0(extd.dir, '/', i))  
    tryCatch(apsimx::apsimx(crop, value = "HarvestReport"), error=function(err) NA)
  }
  newlist<- foreach (i =1:length(my_list_clm)) %dopar% { 
    if(is.na(my_list_sims[i]) == TRUE) {
      setwd(paste0(extd.dir, '/', i)) 
      #tryCatch({my_list_soil$soil$SAT <-c(0.521, 0.521, 0.497, 0.488, 0.478, 0.440)}, error=function(e) {NA})
      my_list_soil[[i]]$SoilName_1$crops <- c("Rice","Wheat","Teff","Sugarcane","Maize","Soybean","OilPalm","Cassava")
      apsimx::edit_apsimx_replace_soil_profile(crop, root = c("pd", "Base_one"), soil.profile = my_list_soil[[i]]$SoilName_1, overwrite = TRUE) 
      my_list_sims[[i]]<-tryCatch(apsimx::apsimx(crop, value = "HarvestReport"), error=function(err) NA)
    }
    else  my_list_sims[[i]]
  }
  
   return(newlist)
}

