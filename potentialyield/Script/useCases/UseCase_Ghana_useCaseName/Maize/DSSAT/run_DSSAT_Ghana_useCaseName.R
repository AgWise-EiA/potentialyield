#################################################################################################################
## Create experimental data in DSSAT format
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/dssat_expfile_zone.R")
prov <- list.files('~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Ghana_useCaseName/Maize/transform/DSSAT/AOI/SC_719')
varieties <- c("SC_719")

for (j in 1:length(varieties)){
  for (i in 1:length(prov)){
    expdata_AOI <- dssat.expfile(country = "Ghana",  useCaseName = "useCaseName", Crop = "Maize",
                                 AOI = FALSE, filex_temp="KEAG8104.MZX", Planting_month_date=NULL,
                                 Harvest_month_date=NULL,season =NULL, plantingWindow=NULL,
                                 ID="TLID",varietyid = "GH0674", zone ="Ahafo", level2="Macanga")
  }
}


dssat.expfile(country="Zambia", useCaseName = "Solidaridad", Crop = "Maize", AOI = TRUE,
                                       filex_temp="KEAG8104.MZX", Planting_month_date="11-01",Harvest_month_date="07-30", 
                                      ID="TLID",season = 1, plantingWindow = 4,varietyid = "999991", zone ="North-Western", level2="Chavuma")



#################################################################################################################
## Run the DSSAT model
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/dssat_exec.R")
execmodel <-dssat.exec(country = "Ghana",  useCaseName = "useCaseName", Crop = "Maize", AOI = FALSE,TRT=1)
# execmodel_AOI <-dssat.exec(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Soybean", AOI = TRUE,TRT=1:5)

#################################################################################################################
## Merge results
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/merge_DSSAT_output.R")
mergeresults <-merge_DSSAT_output(country = "Ghana",  useCaseName = "useCaseName", Crop = "Maize", AOI = FALSE,season=NULL)
# mergeresults_AOI <-merge_DSSAT_output(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Soybean", AOI = TRUE,season=1)
