#################################################################################################################
## Create experimental data in DSSAT format
#################################################################################################################

# source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/Testing/dssat_expfile_provs.R")
# 
# prov <- list.files('~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Mozambique_Solidaridad/Maize/transform/DSSAT/AOI/900111')
# 
# prov <- c("Cabo Delgado", "Manica","Nampula","Tete","Zambezia" )
# varieties <- c("900113")
# 
# for (j in 1:length(varieties)){
#   for (i in 1:length(prov)){
#   expdata_AOI <- dssat.expfile(country = "Mozambique",  useCaseName = "Solidaridad", Crop = "Maize",
#                                AOI = TRUE, filex_temp="KEAG8104.MZX", Planting_month_date="11-01",
#                                Harvest_month_date="05-30", ID="TLID",season =1, plantingWindow=9,
#                                ingenoid=varieties[j],Province = prov[i])
#   }
# }

#################################################################################################################
## Run the DSSAT model
#################################################################################################################

prov <- list.files('~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Mozambique_Solidaridad/Maize/transform/DSSAT/AOI/900111/')
varieties <- c("900111","900112","900113")

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/Testing/dssat_exec.R")
for (j in 1:length(varieties)){
  for (i in 1:length(prov)){
    execmodel_AOI <-dssat.exec(country = "Mozambique",  useCaseName = "Solidaridad", Crop = "Maize",
                               AOI = TRUE,TRT=1:10,ingenoid=varieties[j],Province = prov[i])
  }
}

# setwd("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Zimbabwe_Solidaridad/Maize/transform/DSSAT/AOI/899991/Balaka/EXTE0001/")
# run_dssat(file_name="DSSBatch.v48",suppress_output = TRUE)
# 


#################################################################################################################
## Merge results
#################################################################################################################

# source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/merge_DSSAT_output.R")
# varieties <- c("999991","999992","KY0012")
# 
# for (j in 1:length(varieties)){
#   for (i in 1:length(prov)){
# mergeresults_AOI <-merge_DSSAT_output(country = "Malawi",  useCaseName = "Solidaridad", Crop = "Maize",
#                                       AOI = TRUE,season=1,ingenoid=varieties[j],Province = prov[i])
#   }
# }






