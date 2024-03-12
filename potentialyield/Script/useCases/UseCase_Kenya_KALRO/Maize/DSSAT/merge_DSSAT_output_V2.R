#remotes::install_github("palderman/DSSAT", ref = "develop",force=T)
library("DSSAT")
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @return merge results from DSSAT in RDS format
#' @examples merge_DSSAT_output(country="Rwanda", useCaseName="RAB",Crop="Maize")
prov<-list.files('/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_Kenya_KALRO/Maize/transform/DSSAT/AOI/999991')
ingenoid <- c("999991","999992","KY0012")

output_file<-'useCase_Kenya_KALRO_Maize_AOI_season_1.RDS'
f_all_ingenoid_prov<-NULL
for(v in 1:length(ingenoid)){
 f_all_ingenoid<-NULL
   for(p in 1:length(prov)){  

 path.to.extdata <- paste("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Kenya_KALRO/Maize/transform/DSSAT/AOI/", ingenoid[v],"/", prov[p], sep="")

  setwd(path.to.extdata)
  
  lf <- list.files()
  
  f_all <- NULL
  for (i in 1:length(lf)){
    
    base <- lf[i]

    if(file.exists(paste0(base,"/", base, ".OUT"))==TRUE){
      a <- read_output(paste0(base,"/", base, ".OUT"))
      d <- a[,c("XLAT","LONG","TRNO","TNAM","PDAT", "HDAT","CWAM","HWAH","CNAM","GNAM","NDCH","TMAXA",
                  "TMINA","SRADA","PRCP","ETCP","ESCP")]
      b <- data.frame(d)
      d$XLAT <- b$V15
      d$base <- base

      d$WUE <- d$HWAH / d$PRCP
        
      f_all <- rbind(f_all, d)
    }
  } 
 
  f_all$variety<-ingenoid[v] 
  f_all$prov<-prov[p]
  
  f_all_ingenoid<-rbind(f_all_ingenoid,f_all)
   }
 f_all_ingenoid_prov<-rbind(f_all_ingenoid_prov,f_all_ingenoid)
}
saveRDS(f_all_ingenoid_prov,file=paste0('~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Kenya_KALRO/Maize/result/DSSAT/AOI/',output_file))
  
