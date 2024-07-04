# Merge output of the DSSAT simulations for diverse locations

# Introduction: 
# This script allows Merge output of the DSSAT simulations for diverse locations
# Authors : P.Moreno, , L. Leroux A. Sila, S. Mkuhlani, E.Bendito Garcia 
# Credentials : EiA, 2024
# Last modified July 4, 2024 

#################################################################################################################
## sourcing required packages
#################################################################################################################
packages_required <- c("DSSAT","purrr","mgsub","tidyverse")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))

#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param season when data is needed for more than one season, this needs to be provided to be used in the file name
#' @param varietyid id of the variety based on the cultivar file of DSSAT (column @VAR# in the cultivar file and parameter INGENO in the experimental file *.**X)
#' @param zone_folder When TRUE the output folders are organized by administrative level 1.
#' @param level2_foler When TRUE the output folders are organized by administrative level 2 (has to be part of the administrative level 1 or "zone" of the country) 
#'        for the specific location the experimental file is created
#'        
#' @return merged results from DSSAT in RDS format
#'
#' @examples merge_DSSAT_output(country="Rwanda", useCaseName="RAB",Crop="Maize",varietyid="890011", zone_folder=T, level2_folder=F)
merge_DSSAT_output <- function(country, useCaseName,Crop, AOI=FALSE,season=NULL,varietyid, zone_folder =T, level2_folder=F){


  if (AOI==TRUE){
    if(is.null(season)){
      print("with AOI=TRUE, season can not be null, please refer to the documentation and provide season number")
      return(NULL)
    }
    path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/DSSAT/AOI/",varietyid,sep="")
    
  }else{
    path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/DSSAT/fieldData/",varietyid,sep="")
  }
  if (!dir.exists(file.path(path.to.extdata))){
    print("You need to run the experiments before running the model. Process stopped")
    return(NULL)
  }
  setwd(path.to.extdata)


  a <- list.files(path = path.to.extdata, pattern = "^EXTE.*\\.OUT$", include.dirs=TRUE ,full.names = TRUE, recursive=TRUE)
  
  results <- map_dfr(a, function(.x) {
    tryCatch({
      file <- read_output(.x)
      file <- file[,c("XLAT","LONG","TRNO","TNAM","PDAT", "HDAT","CWAM","HWAH","CNAM","GNAM","NDCH","TMAXA",
                "TMINA","SRADA","PRCP","ETCP","ESCP","CRST")]
      file$file_name <- .x

      #define the name of the zone and level2
      if(level2_folder == T & zone_folder ==T){
        test <- mgsub(.x, c(path.to.extdata, "/EXTE.*"), c("", ""))
        test <- strsplit(test, "/")[[1]]
        test <- test[test != ""]
        file$zone <- test[1]
        file$level2 <-test[2]
      }else if(level2_folder == F & zone_folder ==T){
        file$zone <- mgsub(.x, c(path.to.extdata,"/", "/EXTE.*"), c("","",""))
        file$level2 <- NA
      }else if(level2_folder == T  & zone_folder ==F){
        print("You need to define first a zone (administrative level 1) to be able to run the model for level 2 (administrative level 2). Process stopped")
        return(NULL)
      }else{
        file$zone <- NA
        file$level2 <- NA
      }
      file$variety <- varietyid
      file
    }, error = function(e) {
      cat("Error processing file:", .x, "\n", e$message, "\n")
      NULL  # Return NULL on error to avoid breaking the entire aggregation
    })

  }, .id = "id")
  
  # Specify the results directory path
  if (AOI==TRUE){
    dir_path <-  paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/result/DSSAT/AOI/")
  }else{
    dir_path <-  paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/result/DSSAT/fieldData/")
  }
  # Check if the directory exists
  if (!dir.exists(dir_path)) {
    # Create the directory
    dir.create(dir_path, recursive = TRUE)
  }
  

  saveRDS(results, file = paste0(dir_path, "useCase_", country, "_",useCaseName, "_", Crop,"_variety_",varietyid,"_AOI_season_",season,".rds"))

  
}
  
  
