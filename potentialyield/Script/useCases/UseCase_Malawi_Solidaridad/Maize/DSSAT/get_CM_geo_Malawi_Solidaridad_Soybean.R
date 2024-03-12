

#################################################################################################################
## source "get_rain_temp_summary.R" function and get weather data 
#################################################################################################################
source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/readGeo_CM_V2.R")

#################################################################################################################
## Create soil and weather data in DSSAT format for trial data
#################################################################################################################

#geoData <- readGeo_CM(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Soybean", AOI = FALSE,  Planting_month_date = NULL,Harvest_month_date=NULL,season=NULL,plantingWindow=NULL)

#################################################################################################################
## Create soil and weather data in DSSAT format for AOI data
#################################################################################################################
country <- "Malawi"
countryShp <- geodata::gadm(country, level = 2, path='.')
prov <- unique(countryShp$NAME_1)

for (i in 1:length(prov)){
  geoData_AOI <- readGeo_CM(country="Malawi",  useCaseName = "Solidaridad", Crop = "Soybean", AOI = TRUE, season=1, Province = prov[i])}

