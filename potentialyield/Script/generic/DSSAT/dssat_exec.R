
packages = c("tidyverse", "DSSAT")
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
#'
#' @return DSSAT outputs
#' @export
#'
#' @examples rundssat(1)

rundssat <-function(i){
  setwd(paste(path.to.extdata,paste0('EXTE', formatC(width = 4, as.integer((i)), flag = "0")), sep = "/"))
  # Generate a DSSAT batch file using a tibble
  options(DSSAT.CSM="sudo /opt/DSSAT/dscsm047")
  tibble(FILEX=paste0('EXTE', formatC(width = 4, as.integer((i)), flag = "0"),'.MZX'), TRTNO=TRT, RP=1, SQ=0, OP=0, CO=0) %>%
    write_dssbatch()
  # Run DSSAT-CSM
  run_dssat(suppress_output = TRUE)
  # Change output file name
  file.rename(list.files(pattern = "Summary.*", full.names = TRUE), paste0(path.to.extdata, '/', 'EXTE', formatC(width = 4, as.integer((i)), flag = "0"), '/', 'EXTE', formatC(width = 4, as.integer((i)), flag = "0"), '.OUT'))
  setwd(path.to.extdata)
  gc()
}

#' Main function that define the files to run DSSAT
#'
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param Planting_month_date is needed only for AOI and should be provided as month_date, for trial locations the actual planting date is be used so no need to change the default value
#' @param TRT is the number of treatments to run from the experimental file
#'
#' @return
#' @export
#'
#' @examples dssat.exec(country = "Rwanda",  useCaseName = "RAB", Crop = "Maize", AOI = FALSE, Planting_month_date = NULL,jobs=10,TRT=1:36)


dssat.exec <- function(country, useCaseName, Crop, AOI = FALSE, Planting_month_date=NULL,jobs=10,TRT=1){  
  
  #require(doParallel)
  #require(foreach)
  # Set number of parallel workers
  #cls <- parallel::makePSOCKcluster(jobs)
  #doParallel::registerDoParallel(cls)
  #Set working directory to save the results
  path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/result/DSSAT", sep="")
  setwd(path.to.extdata)
  
  folders <- list.dirs(".", full.names = FALSE, recursive = FALSE)
  matching_folders <- folders[grepl("EXTE", folders, ignore.case = TRUE)]
  #foreach::foreach(i=seq_along(matching_folders), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "DSSAT")) %dopar% {
 
  results <- map(seq_along(matching_folders), rundssat) %||% print("Progress:")
}