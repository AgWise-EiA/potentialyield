
dssat.exec <- function(jobs,ex.name,path.to.extdata){
  require(doParallel)
  require(foreach)
  # Set number of parallel workers
  cls <- parallel::makePSOCKcluster(jobs)
  doParallel::registerDoParallel(cls)
  #Set working directory (where the file is)
  setwd(path.to.extdata)
  
  # Create grid
  coords <- read.csv("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_Rwanda_DSSAT/Climate_data/coordinates_Rwanda.csv")
  # grid <- as.matrix(coords[c(1:8),])
  grid <- as.matrix(coords)
  
  # Process Experimental Files
  foreach::foreach(i=seq_along(grid[,1]), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "DSSAT")) %dopar% {
    # Set the experimental directory
    setwd(paste(path.to.extdata,ex.name,paste0('EXTE', formatC(width = 4, as.integer((i-1)), flag = "0")), sep = "/"))
    # Generate a DSSAT batch file using a tibble
    options(DSSAT.CSM="sudo /opt/DSSAT/dscsm047")
    tibble(FILEX=paste0('EXTE', formatC(width = 4, as.integer((i-1)), flag = "0"),'.MZX'), TRTNO=1:36, RP=1, SQ=0, OP=0, CO=0) %>%
      write_dssbatch()
    # Run DSSAT-CSM
    run_dssat(suppress_output = TRUE)
    # Change output file name
    file.rename(list.files(pattern = "Summary.*", full.names = TRUE), paste0(path.to.extdata, '/', ex.name, '/', 'EXTE', formatC(width = 4, as.integer((i-1)), flag = "0"), '/', 'EXTE', formatC(width = 4, as.integer((i-1)), flag = "0"), '.OUT'))
    setwd(path.to.extdata)
    gc()
  }
}