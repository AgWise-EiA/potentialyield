#################################################################################################################
## Create experimental data in DSSAT format
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/dssat_expfile.R")
# expdata <- dssat.expfile(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Soybean", AOI = TRUE, filex_temp="ESAC8501.SBX", Planting_month_date = NULL,Harvest_month_date=NULL,ID="TLID",season =NULL, plantingWindow=NULL)

expdata_AOI <- dssat.expfile(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Soybean", AOI = TRUE, filex_temp="ESAD8501.SBX", Planting_month_date="10-15", Harvest_month_date="05-15", ID="TLID",season =1, plantingWindow=7, ingenoid="IB0058")

#################################################################################################################
## Run the DSSAT model
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/dssat_exec.R")
# execmodel <-dssat.exec(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Soybean", AOI = FALSE,TRT=1)
execmodel_AOI <-dssat.exec(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Soybean", AOI = TRUE,TRT=1:8)

#################################################################################################################
## Merge results
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/merge_DSSAT_output.R")
mergeresults <-merge_DSSAT_output(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Soybean", AOI = TRUE,season=1)
mergeresults_AOI <-merge_DSSAT_output(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Soybean", AOI = TRUE,season=1)
