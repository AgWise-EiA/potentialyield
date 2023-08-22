
dssat.expfile <- function(jobs,ex.name,path.to.extdata){  #xmin,xmax,ymin,ymax,res,jobs,ex.name,path.to.extdata){
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
    setwd(path.to.extdata)
    # #Read sample cultivar file and filter to only cultivar IF0014
    # cul <- DSSAT::read_cul("PTSUB047.CUL") %>%
    #   filter(`VAR#` == "IB0005")
    # #Read sample cultivar file and filter to only ecotype IB0002
    # eco <- DSSAT::read_eco("PTSUB047.ECO") %>%
    #   filter(`ECO#` == "IB0001")
    #Read in original FileX
    file_x <- DSSAT::read_filex("MZRL8142.MZX")
    #Set the experimental directory
    setwd(paste(path.to.extdata,ex.name,paste0('EXTE', formatC(width = 4, as.integer((i-1)), flag = "0")), sep = "/"))
    #Make proposed changes to FileX
    file_x$FIELDS$WSTA<-paste0("WHTE", formatC(width = 4, as.integer((i-1)), flag = "0"))
    file_x$FIELDS$ID_SOIL<-paste0('TRAN', formatC(width = 6, as.integer((i-1)), flag = "0"))
    #Overwrite original FileX with new values
    DSSAT::write_filex(file_x,paste0('EXTE', formatC(width = 4, as.integer((i-1)), flag = "0"),'.MZX'))
    setwd(path.to.extdata)
    gc()
  }
}
