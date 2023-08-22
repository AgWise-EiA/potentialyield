
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#'
#' @return merge results from DSSAT in RDS format
#'
#' @examples merge_DSSAT_output(country="Rwanda", useCaseName="RAB",Crop="Maize")
merge_DSSAT_output <- function(country, useCaseName,Crop){
  
  # Set number of parallel workers
  #cls <- parallel::makePSOCKcluster(jobs)
  #doParallel::registerDoParallel(cls)
  
  path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/result/DSSAT", sep="")
  setwd(path.to.extdata)
  

  lf <- list.files()
  
  f_all <- NULL
  for (i in 1:length(lf)){
    
    base <- lf[i]
    basew <- gsub("EX","WH", base)
    
    
    if(file.exists(paste0(base,"/", basew, ".WTH"))==TRUE){
      e <- readLines(paste0(base,"/", basew, ".WTH"), n=5)[5]
    }
    
    e <- strsplit(e, " ", fixed=TRUE) 
    Lat <- e[[1]][6]
    Lon <- e[[1]][9]
    
    
    if(file.exists(paste0(base,"/", base, ".OUT"))==TRUE){
      a <- read.table(paste0(base,"/", base, ".OUT"), skip = 4, header = F)
      b <- data.frame(a)
      c <- b[order(b[,15], decreasing = FALSE), ]
      d <- c[,c(15,19,21,23,47,48,79,80,81,84,85,86)]
      colnames(d) <- c('planting.date','harvesting.date','Total.aboveground.biomass(kg/ha)','WLY(kg/ha)',
                       'Total.aboveground.bio.N%(kg/ha)','GrainNMaturity(kg/ha)','Av.Tmax(°C)',
                       'Av.Tmin(°C)','A.Solar.rad(MJ/m2/d)','Total.Seasonal.Rainfall(mm)',
                       'Total.Seasonal.ETranspiration(mm)','Total.Seasonal.Soil.Evaporation(mm)')
      
      d$crop.duration <- d$harvesting.date - d$planting.date
      d$WUE <- d$`WLY(kg/ha)` / d$`Total.Seasonal.Rainfall(mm)`
      d$Lon <- Lon
      d$Lat <- Lat
      
      f <- d[,c("Lon","Lat","planting.date", "harvesting.date","crop.duration","WLY(kg/ha)","Total.aboveground.biomass(kg/ha)","WUE",
                "GrainNMaturity(kg/ha)","Total.aboveground.bio.N%(kg/ha)","Av.Tmax(°C)","Av.Tmin(°C)","A.Solar.rad(MJ/m2/d)",
                "Total.Seasonal.Rainfall(mm)","Total.Seasonal.ETranspiration(mm)","Total.Seasonal.Soil.Evaporation(mm)")]
      f_all <- rbind(f_all, f)
    }
    
    
  } 
  saveRDS(f_all, file = paste0(path.to.extdata,"/useCase_", country, "_",useCaseName, "_", Crop,".rds"))
  
  
}