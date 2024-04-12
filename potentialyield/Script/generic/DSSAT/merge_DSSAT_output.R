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
library()
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param season when data is needed for more than one season, this needs to be provided to be used in the file name
#' @param varietyid id of the variety based on the cultivar file of DSSAT (column @VAR# in the cultivar file and parameter INGENO in the experimental file *.**X)
#' @return merged results from DSSAT in RDS format
#'
#' @examples merge_DSSAT_output(country="Rwanda", useCaseName="RAB",Crop="Maize")
merge_DSSAT_output <- function(country, useCaseName,Crop, AOI=FALSE,season=NULL,varietyid){

  if (AOI==TRUE){
    if(is.null(season)){
      print("with AOI=TRUE, season can not be null, please refer to the documentation and provide season number")
      return(NULL)
    }
    path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/DSSAT/AOI/",varietyid,sep="")
    
  }else{
    path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/DSSAT/fieldData/",varietyid,sep="")
  }
  setwd(path.to.extdata)


  a <- list.files(path = path.to.extdata, pattern = "^EXTE.*\\.OUT$", include.dirs=TRUE ,full.names = TRUE, recursive=TRUE)
  
  results <- map_dfr(a, function(.x) {
    tryCatch({
      file <- read_output(.x)
      file <- file[,c("XLAT","LONG","TRNO","TNAM","PDAT", "HDAT","CWAM","HWAH","CNAM","GNAM","NDCH","TMAXA",
                "TMINA","SRADA","PRCP","ETCP","ESCP","CRST")]
      file$file_name <- .x
      if (AOI==TRUE){
        test <- mgsub(.x, c(path.to.extdata, "/EXTE.*"), c("", ""))
        test <- strsplit(test, "/")[[1]]
        test <- test[test != ""]
        file$zone <- test[1]
        file$level2 <-test[2]
      }else{
        file$zone <- mgsub(.x, c(path.to.extdata,"/", "/EXTE.*"), c("","",""))
      }
      file
    }, error = function(e) {
      cat("Error processing file:", .x, "\n", e$message, "\n")
      NULL  # Return NULL on error to avoid breaking the entire aggregation
    })

  }, .id = "id")
  
  if (AOI==TRUE){
    saveRDS(results, file = paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/result/DSSAT/AOI/useCase_", country, "_",useCaseName, "_", Crop,"_variety_",varietyid,"_AOI_season_",season,".rds"))
    
  }else{
    saveRDS(results, file = paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/result/DSSAT/useCase_", country, "_",useCaseName, "_", Crop,"_variety_",varietyid,".rds"))
    
  }
  
}
  
  
 
  
  
  
#   provs <- list.files(full.names = TRUE)
#   
#   p_all <- NULL
#   
#   for (p in 1:length(provs)){
#     
#   lf <- list.files(provs[p])
#   
#   f_all <- NULL
#   
#   for (i in 1:length(lf)){
#     
#     base <- lf[i]
# 
#     if(file.exists(paste0(provs[p], '/', base,"/", base, ".OUT"))==TRUE){
#       a <- read_output(paste0(provs[p], '/',base,"/", base, ".OUT"))
#       d <- a[,c("XLAT","LONG","TRNO","TNAM","PDAT", "HDAT","CWAM","HWAH","CNAM","GNAM","NDCH","TMAXA",
#                   "TMINA","SRADA","PRCP","ETCP","ESCP")]
#       # b <- read.table(paste0(base,"/", base, ".OUT"), skip = 4, header = F)
#       b <- data.frame(d)
#       # d$XLAT <- b$V14
#       # d$XLON <- b$V15
#       d$base <- base
#         
#         # colnames(d) <- c('latitude','longitude','treatment.number','treatment.name','planting.date','harvesting.date','Total.aboveground.biomass(kg/ha)','WLY(kg/ha)',
#         #                  'Total.aboveground.bio.N%(kg/ha)','GrainNMaturity(kg/ha)','crop.duration','Av.Tmax(°C)',
#         #                  'Av.Tmin(°C)','A.Solar.rad(MJ/m2/d)','Total.Seasonal.Rainfall(mm)',
#         #                  'Total.Seasonal.ETranspiration(mm)','Total.Seasonal.Soil.Evaporation(mm)')
#         
#       d$WUE <- d$HWAH / d$PRCP
#         
#       f_all <- rbind(f_all, d)
#     }
#     
#     p_all <- rbind(p_all, f_all)
#   }
#   p_all <- unique(p_all)
#   } 
#   if (AOI==TRUE){
#     saveRDS(p_all, file = paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/result/DSSAT/AOI/useCase_", country, "_",useCaseName, "_", Crop,"_AOI_season_",season,".rds"))
#       
#   }else{
#     saveRDS(p_all, file = paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/result/DSSAT/useCase_", country, "_",useCaseName, "_", Crop,".rds"))
#     
#   }
#   
# }
