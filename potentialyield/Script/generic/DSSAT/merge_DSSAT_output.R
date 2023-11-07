
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#'
#' @return merge results from DSSAT in RDS format
#'
#' @examples merge_DSSAT_output(country="Rwanda", useCaseName="RAB",Crop="Maize")
merge_DSSAT_output <- function(country, useCaseName,Crop, AOI=FALSE,season=NULL){
  
  # Set number of parallel workers
  #cls <- parallel::makePSOCKcluster(jobs)
  #doParallel::registerDoParallel(cls)
  if (AOI==TRUE){
    if(is.null(season)){
      print("with AOI=TRUE, season can not be null, please refer to the documentation and provide season number")
      return(NULL)
    }
    path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/DSSAT/AOI", sep="")
  }else{
    path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/DSSAT", sep="")
  }
  setwd(path.to.extdata)
  

  lf <- list.files()
  
  f_all <- NULL
  for (i in 1:length(lf)){
    
    base <- lf[i]

    if(file.exists(paste0(base,"/", base, ".OUT"))==TRUE){
      a <- read_output(paste0(base,"/", base, ".OUT"))
      d <- a[,c("XLAT","LONG","TRNO","TNAM","PDAT", "HDAT","CWAM","HWAH","CNAM","GNAM","NDCH","TMAXA",
                  "TMINA","SRADA","PRCP","ETCP","ESCP")]
      b <- read.table(paste0(base,"/", base, ".OUT"), skip = 4, header = F)
      b <- data.frame(b)
      d$XLAT <- b$V15
      d$base <- base
        
        # colnames(d) <- c('latitude','longitude','treatment.number','treatment.name','planting.date','harvesting.date','Total.aboveground.biomass(kg/ha)','WLY(kg/ha)',
        #                  'Total.aboveground.bio.N%(kg/ha)','GrainNMaturity(kg/ha)','crop.duration','Av.Tmax(°C)',
        #                  'Av.Tmin(°C)','A.Solar.rad(MJ/m2/d)','Total.Seasonal.Rainfall(mm)',
        #                  'Total.Seasonal.ETranspiration(mm)','Total.Seasonal.Soil.Evaporation(mm)')
        
      d$WUE <- d$HWAH / d$PRCP
        
      f_all <- rbind(f_all, d)
    }
    
    
  } 
  if (AOI==TRUE){
    saveRDS(f_all, file = paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/result/DSSAT/AOI/useCase_", country, "_",useCaseName, "_", Crop,"_AOI_season_",season,".rds"))
  }else{
    saveRDS(f_all, file = paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/result/DSSAT/useCase_", country, "_",useCaseName, "_", Crop,".rds"))
    
  }
  
  
}