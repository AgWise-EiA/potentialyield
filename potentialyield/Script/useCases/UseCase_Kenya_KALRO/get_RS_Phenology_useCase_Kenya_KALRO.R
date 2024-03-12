#################################################################################################################
## source "get_RS_Phenology.R" function and execute it for Kenya KALRO use case
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/RemoteSensing/get_RS_Phenology.R")
Planting_year <- seq(2017,2020, by=1)
Harvesting_year <- Planting_year

for (i in 1:length(Planting_year)){
  print (i)
  Phenology_rasterTS(country = "Kenya", useCaseName = "KALRO",Planting_year = Planting_year[i], Harvesting_year = Harvesting_year[i], Planting_month = "February",Harvesting_month = "September", overwrite = TRUE)
}
