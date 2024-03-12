#################################################################################################################
## source "get_RS_Phenology.R" function and execute it for Rwanda RAB use case
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/RemoteSensing/get_RS_Phenology.R")
Phenology_rasterTS(country = "Rwanda", useCaseName = "RAB",Planting_year = 2021, Harvesting_year = 2021, Planting_month = "February",Harvesting_month = "July", overwrite = TRUE)
