


#################################################################################################################
## source "get_rain_temp_summary.R" function and get weather data 
#################################################################################################################
source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/readGeo_CM.R")

#################################################################################################################
## Create soil and weather data in DSSAT format
#################################################################################################################

geoData <- readGeo_CM(country = "Rwanda",  useCaseName = "RAB", Crop = "Maize", AOI = FALSE,  Planting_month_date = NULL,jobs=30)



