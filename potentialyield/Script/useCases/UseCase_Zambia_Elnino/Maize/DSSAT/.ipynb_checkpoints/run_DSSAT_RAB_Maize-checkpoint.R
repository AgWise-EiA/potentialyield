#################################################################################################################
## Create experimental data in DSSAT format
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/dssat_expfile.R")
expdata <- dssat.expfile(country = "Rwanda",  useCaseName = "RAB", Crop = "Maize", AOI = FALSE, filex_temp="MZRM8143.MZX", Planting_month_date = NULL,Harvest_month_date=NULL,jobs=10, ID="TLID")

#################################################################################################################
## Run the DSSAT model
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/dssat_exec.R")
execmodel <-dssat.exec(country = "Rwanda",  useCaseName = "RAB", Crop = "Maize", AOI = FALSE, Planting_month_date = NULL,jobs=10,TRT=1)

#################################################################################################################
## Merge results
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/merge_DSSAT_output.R")
mergeresults <-merge_DSSAT_output(country = "Rwanda",  useCaseName = "RAB", Crop = "Maize")

