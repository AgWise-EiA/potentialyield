#' Create the experimental file
#'
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param Planting_month_date is needed only for AOI and should be provided as month_date, for trial locations the actual planting date is be used so no need to change the default value
#'
#' @return
#' @export
#'
#' @examples dssat.expfile(country = "Rwanda",  useCaseName = "RAB", Crop = "Maize", AOI = FALSE, filex_temp="MZRL8142.MZX", Planting_month_date = NULL,jobs=10)


dssat.expfile <- function(country, useCaseName, Crop, AOI = FALSE,filex_temp="MZRL8142.MZX", Planting_month_date=NULL,jobs=10){  #xmin,xmax,ymin,ymax,res,jobs,ex.name,path.to.extdata){
  
  #Set working directory to save the results
  path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/result/DSSAT", sep="")
  
  #Define working directory with template data
  path.to.temdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/Landing/DSSAT", sep="")
  #We need to add more codes
  crops <- c("Maize","Potato")
  cropcode <- c("MZ","PT")
  
  cropid <- which(crops == Crop)
  code <- cropcode[cropid]
  
  require(doParallel)
  require(foreach)
  # Set number of parallel workers
  cls <- parallel::makePSOCKcluster(jobs)
  doParallel::registerDoParallel(cls)
  #Set working directory (where the file is)
  setwd(path.to.extdata)
  
  folders <- list.dirs(".", full.names = FALSE, recursive = FALSE)
  matching_folders <- folders[grepl("EXTE", folders, ignore.case = TRUE)]
  
  # Process Experimental Files
  foreach::foreach(i=seq_along(matching_folders), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "DSSAT")) %dopar% {
    setwd(path.to.temdata)

    #Read in original FileX
    file_x <- DSSAT::read_filex(filex_temp)
    #Set the experimental directory
    setwd(paste(path.to.extdata,paste0('EXTE', formatC(width = 4, as.integer((i)), flag = "0")), sep = "/"))
    #Make proposed changes to FileX
    file_x$FIELDS$WSTA<-paste0("WHTE", formatC(width = 4, as.integer((i)), flag = "0"))
    file_x$FIELDS$ID_SOIL<-paste0('TRAN', formatC(width = 5, as.integer((i)), flag = "0"))
    file_x$CULTIVARS$CR <- code
    #Overwrite original FileX with new values
    DSSAT::write_filex(file_x,paste0('EXTE', formatC(width = 4, as.integer((i)), flag = "0"),'.',code,'X'))
    gc()
  }
}