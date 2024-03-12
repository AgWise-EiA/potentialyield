#################################################################################################################
## source "get_RS_Croptype.R" functions and execute it for Rwanda RAB Maize use case
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/RemoteSensing/get_RS_Croptype.R")

country = "Rwanda"
useCaseName = "RAB"
Planting_year = 2021
Harvesting_year = 2022
Planting_month = "August"
Harvesting_month = "February"
overwrite = TRUE
crop = c("Maize")
coord = c("lon", "lat")

CropType (country, useCaseName, Planting_year, Harvesting_year, Planting_month, Harvesting_month, crop, coord, overwrite)