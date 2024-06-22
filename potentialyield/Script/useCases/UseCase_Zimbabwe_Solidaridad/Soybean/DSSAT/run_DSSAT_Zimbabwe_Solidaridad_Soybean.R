#################################################################################################################
## Create experimental data in DSSAT format
#################################################################################################################
 
# source('~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/Testing/dssat_expfile_provs_V2.R')
# # source('~/AgWise_SM/dssat_expfile_provs.R')
# 
# prov <- list.files('~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Zimbabwe_Solidaridad/Soybean/transform/DSSAT/AOI/999911')
# varieties <- c("999911","999912","999913")
# 
# for (j in 1:length(varieties)){
#   for (i in 1:length(prov)){
#   expdata_AOI <- dssat.expfile(country = "Zimbabwe",  useCaseName = "Solidaridad", Crop = "Soybean",
#                                AOI = TRUE, filex_temp="ESAD8501.SBX", Planting_month_date="12-01",
#                                Harvest_month_date="05-30", ID="TLID",season =1, plantingWindow=8,
#                                ingenoid=varieties[j],Province = prov[i])
#   }
# }

#################################################################################################################
## Run the DSSAT model
#################################################################################################################


prov <- list.files('~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Zimbabwe_Solidaridad/Soybean/transform/DSSAT/AOI/999911')
varieties <- c("999911","999912","999913")

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/Testing/dssat_exec_sb.R")
for (j in 1:length(varieties)){
  for (i in 1:length(prov)){
    execmodel_AOI <-dssat.exec(country = "Zimbabwe",  useCaseName = "Solidaridad", Crop = "Soybean",
                               AOI = TRUE,TRT=1:9,ingenoid=varieties[j],Province = prov[i])
  }
}








# setwd("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Zimbabwe_Solidaridad/Soybean/transform/DSSAT/AOI/880002/Bulawayo/EXTE0001/")
# run_dssat(run_mode = "B",file_name="DSSBatch.v48",suppress_output = FALSE)
# # 


#################################################################################################################
## Merge results
#################################################################################################################

# source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/merge_DSSAT_output.R")
# varieties <- c("999991","999992","KY0012")
# 
# for (j in 1:length(varieties)){
#   for (i in 1:length(prov)){
# mergeresults_AOI <-merge_DSSAT_output(country = "Malawi",  useCaseName = "Solidaridad", Crop = "Soybean",
#                                       AOI = TRUE,season=1,ingenoid=varieties[j],Province = prov[i])
#   }
# }






