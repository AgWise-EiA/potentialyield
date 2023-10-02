#################################################################################################################
## Create experimental data in DSSAT format
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/dssat_expfile.R")
expdata <- dssat.expfile(country = "Nigeria",  useCaseName = "AKILIMO", Crop = "Maize", AOI = FALSE, filex_temp="CIAK0001.MZX", Planting_month_date = NULL,Harvest_month_date=NULL,ID="ID",season =NULL, plantingWindow=NULL)


#################################################################################################################
## Run the DSSAT model
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/dssat_exec.R")
execmodel <-dssat.exec(country = "Rwanda",  useCaseName = "RAB", Crop = "Maize", AOI = FALSE,TRT=1)
execmodel_AOI <-dssat.exec(country = "Rwanda",  useCaseName = "RAB", Crop = "Maize", AOI = TRUE,TRT=1:5)

#################################################################################################################
## Merge results
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/merge_DSSAT_output.R")
mergeresults <-merge_DSSAT_output(country = "Rwanda",  useCaseName = "RAB", Crop = "Maize", AOI = FALSE,season=NULL)
mergeresults_AOI <-merge_DSSAT_output(country = "Rwanda",  useCaseName = "RAB", Crop = "Maize", AOI = TRUE,season=1)
