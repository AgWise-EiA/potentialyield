#################################################################################################################
## Create experimental data in DSSAT format
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/dssat_expfile.R")
expdata <- dssat.expfile(country = "Rwanda",  useCaseName = "RAB", Crop = "Maize", AOI = FALSE, filex_temp="MZRM8143.MZX", Planting_month_date = NULL,Harvest_month_date=NULL,jobs=10, ID="TLID",season =NULL, plantingWindow=NULL)

expdata_AOI <- dssat.expfile(country = "Rwanda",  useCaseName = "RAB", Crop = "Maize", AOI = TRUE, filex_temp="MZRM8143.MZX", Planting_month_date="08-15", Harvest_month_date="03-31",jobs=10, ID="TLID",season =1, plantingWindow=4)

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
