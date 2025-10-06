source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/APSIM/04_merge_APSIM_output.R")

country = "Rwanda"
useCaseName = "RAB"
Crop = "Maize"
AOI = TRUE
season = 1
varietyids = c("Early")

merge_APSIM_output(country, useCaseName, Crop, AOI, season, varietyids)