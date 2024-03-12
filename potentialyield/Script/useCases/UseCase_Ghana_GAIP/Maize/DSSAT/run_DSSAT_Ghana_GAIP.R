#################################################################################################################
## Create experimental data in DSSAT format
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/dssat_expfile.R")
expdata <- dssat.expfile(country = "Ghana",  useCaseName = "GAIP", Crop = "Maize", AOI = FALSE, filex_temp="ESAC8501.SBX", Planting_month_date = NULL,Harvest_month_date=NULL,jobs=10, ID="TLID",season =NULL, plantingWindow=NULL)

# expdata_AOI <- dssat.expfile(country = "Ghana",  useCaseName = "GAIP", Crop = "Maize", AOI = TRUE, filex_temp="ESAC8501.SBX", Planting_month_date="10-15", Harvest_month_date="05-15",jobs=10, ID="TLID",season =1, plantingWindow=10)

#################################################################################################################
## Run the DSSAT model
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/dssat_exec.R")
execmodel <-dssat.exec(country = "Ghana",  useCaseName = "GAIP", Crop = "Maize", AOI = FALSE,TRT=1)
# execmodel_AOI <-dssat.exec(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Soybean", AOI = TRUE,TRT=1:5)

#################################################################################################################
## Merge results
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/merge_DSSAT_output.R")
mergeresults <-merge_DSSAT_output(country = "Ghana",  useCaseName = "GAIP", Crop = "Maize", AOI = FALSE,season=NULL)
# mergeresults_AOI <-merge_DSSAT_output(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Soybean", AOI = TRUE,season=1)
