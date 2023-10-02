


#################################################################################################################
## source "get_rain_temp_summary.R" function and get weather data 
#################################################################################################################
source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/readGeo_CM.R")

#################################################################################################################
## Create soil and weather data in DSSAT format for trial data
#################################################################################################################

geoData <- readGeo_CM(country = "Nigeria",  useCaseName = "AKILIMO", Crop = "Maize", AOI = FALSE)

