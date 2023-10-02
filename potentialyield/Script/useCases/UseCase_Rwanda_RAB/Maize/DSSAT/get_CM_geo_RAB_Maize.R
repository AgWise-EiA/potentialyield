


#################################################################################################################
## source "get_rain_temp_summary.R" function and get weather data 
#################################################################################################################
source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/readGeo_CM.R")

#################################################################################################################
## Create soil and weather data in DSSAT format for trial data
#################################################################################################################

geoData <- readGeo_CM(country = "Rwanda",  useCaseName = "RAB", Crop = "Maize", AOI = FALSE,  Planting_month_date = NULL,Harvest_month_date=NULL,season=NULL,plantingWindow=NULL)

#################################################################################################################
## Create soil and weather data in DSSAT format for AOI data
#################################################################################################################

geoData_AOI <- readGeo_CM(country="Rwanda",  useCaseName = "RAB", Crop = "Maize", AOI = TRUE, Planting_month_date="08-15", Harvest_month_date="03-31", season=1, plantingWindow=4)
