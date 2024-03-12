#################################################################################################################
## Create experimental data in DSSAT format
#################################################################################################################
# 
source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/dssat_expfile.R")
prov <- list.files('~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Zambia_Solidaridad/Soybean/transform/DSSAT/AOI/880002')
varieties <- c("880002","880004","899996")

for (j in 1:length(varieties)){
  for (i in 1:length(prov)){
  expdata_AOI <- dssat.expfile(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Soybean",
                               AOI = TRUE, filex_temp="KEAG8104.MZX", Planting_month_date="12-01",
                               Harvest_month_date="05-30", ID="TLID",season =1, plantingWindow=8,
                               ingenoid=varieties[j],Province = prov[i])
  }
}

#################################################################################################################
## Run the DSSAT model
#################################################################################################################

# 
# prov <- list.files('~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Malawi_Solidaridad/Maize/transform/DSSAT/AOI/899991')
# varieties <- c("999991","999992","890002")
# 
# source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/dssat_exec.R")
# for (j in 1:length(varieties)){
#   for (i in 1:length(prov)){
#     execmodel_AOI <-dssat.exec(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Maize",
#                                AOI = TRUE,TRT=1:9,ingenoid=varieties[j],Province = prov[i])
#   }
# }
# 

#################################################################################################################
## Merge results
#################################################################################################################

# source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/merge_DSSAT_output.R")
# varieties <- c("999991","999992","890002")
# 
# for (j in 1:length(varieties)){
#   for (i in 1:length(prov)){
# mergeresults_AOI <-merge_DSSAT_output(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Maize",
#                                       AOI = TRUE,season=1,ingenoid=varieties[j],Province = prov[i])
#   }
# }






