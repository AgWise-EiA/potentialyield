


#################################################################################################################
## source "get_rain_temp_summary.R" function and get weather data 
#################################################################################################################
source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/readGeo_CM.R")

#################################################################################################################
## Create soil and weather data in DSSAT format for trial data
#################################################################################################################

# geoData <- readGeo_CM(country = "Zambia",  useCaseName = "Elnino", Crop = "Maize", AOI = TRUEE,  Planting_month_date = NULL,Harvest_month_date=NULL,season=NULL,plantingWindow=NULL)
geoData <- readGeo_CM(country = "Zambia",  useCaseName = "Elnino", Crop = "Maize", AOI = FALSE,  season=1)

#################################################################################################################
## Create soil and weather data in DSSAT format for AOI data
#################################################################################################################

geoData_AOI <- readGeo_CM(country="Zambia",  useCaseName = "Elnino", Crop = "Maize", AOI = TRUE, Planting_month_date="11-01", Harvest_month_date="03-31", season=1, plantingWindow=16)
