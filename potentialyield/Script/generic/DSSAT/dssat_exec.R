# Create DSSAT Batch file and run the model

# Introduction: 
# This script allows the creation of Batch file and run the model
# Authors : P.Moreno, A. Sila, S. Mkuhlani, E.Bendito Garcia 
# Credentials : EiA, 2024
# Last modified April 08, 2024 

packages_required <- c("tidyverse", "DSSAT")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))

#' Individual function to run DSSAT
#'
#' @param i run number
#' @param path.to.extdata Main folder where the results of the simulations are going to be stored.
#' @param TRT number of treatments to be run from the experimental file
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param crop_code Code of the crop in DSSAT (e.g., MZ for maize) created in the function dssat.exec.
#' @param zone Name of the administrative level 1 for the specific location the experimental file is created.
#' @param level2 Name of the administrative level 2 (has to be part of the administrative level 1 or "zone" of the country) 
#'        for the specific location the experimental file is created
#'
#' @return DSSAT outputs
#' @export
#'
#' @examples rundssat(1)

rundssat <-function(i,path.to.extdata,TRT,AOI=T,crop_code,zone,level2){
  if(AOI==TRUE){
    setwd(paste(path.to.extdata,paste0(zone,"/",level2,'/EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/"))
  } else{
    setwd(paste(path.to.extdata,paste0(zone,"/",'EXTE', formatC(width = 4, as.integer((i)), flag = "0")), sep = "/"))
  }


  
  # Generate a DSSAT batch file using a tibble
  options(DSSAT.CSM="/opt/DSSAT/v4.8.1.40/dscsm048")
  tibble(FILEX=paste0('EXTE', formatC(width = 4, as.integer((i)), flag = "0"),'.',crop_code,'X'), TRTNO=TRT, RP=1, SQ=0, OP=0, CO=0) %>%
    write_dssbatch(file_name="DSSBatch.v48")
  # Run DSSAT-CSM
  run_dssat(file_name="DSSBatch.v48",suppress_output = TRUE)
  # Change output file name
  file.rename("Summary.OUT", paste0(path.to.extdata, '/', 'EXTE', formatC(width = 4, as.integer((i)), flag = "0"), '/', 'EXTE', formatC(width = 4, as.integer((i)), flag = "0"), '.OUT'))
  gc()
}

#' Main function that define the files to run DSSAT
#'
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param TRT is the number of treatments to be run from the experimental file
#' @param varietyid identification or variety ID in the cultivar file of DSSAT
#' @param zone Name of the administrative level 1 for the specific location the experimental file is created.
#' @param level2 Name of the administrative level 2 (has to be part of the administrative level 1 or "zone" of the country) 
#'        for the specific location the experimental file is created
#' @return
#' @export
#'
#' @examples dssat.exec(country = "Rwanda",  useCaseName = "RAB", Crop = "Maize", AOI = FALSE, Planting_month_date = NULL,jobs=10,TRT=1:36)


 dssat.exec <- function(country, useCaseName, Crop, AOI = FALSE,TRT,varietyid, zone, level2){  
     
  #Set working directory to save the results
  if (AOI==TRUE){
    path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/DSSAT/AOI/", varietyid,"/", sep="")
    }else{
    path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/DSSAT/fieldData/",varietyid,"/", sep="")
  }
  
  setwd(path.to.extdata)
  
  folders <- list.dirs(".", full.names = FALSE, recursive = FALSE)
  matching_folders <- folders[grepl("EXTE", folders, ignore.case = TRUE)]
  
  crops <- c("Maize", "Potato", "Rice", "Soybean", "Wheat")
  cropcode_supported <- c("MZ","PT", "RI", "SB", "WH")
  
  cropid <- which(crops == Crop)
  crop_code <- cropcode_supported[cropid]
 
  results <- map(seq_along(matching_folders), rundssat,path.to.extdata=path.to.extdata,TRT=TRT, AOI=AOI,crop_code=crop_code,zone=zone,level2=level2) %||% print("Progress:")
}
