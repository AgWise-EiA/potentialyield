

#################################################################################################################
## source "get_rain_temp_summary.R" function and get weather data 
#################################################################################################################
#source("~/patricia-potentialyield/potentialyield/potentialyield/Script/generic/DSSAT/readGeo_CM_V2.R")
source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/readGeo_CM_V2.R")


#################################################################################################################
## Create soil and weather data in DSSAT format for trial data
#################################################################################################################

#geoData <- readGeo_CM(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Soybean", AOI = FALSE,  Planting_month_date = NULL,Harvest_month_date=NULL,season=NULL,plantingWindow=NULL)

#################################################################################################################
## Create soil and weather data in DSSAT format for AOI data
#################################################################################################################
country <- "Kenya"
countryShp <- geodata::gadm(country, level = 2, path='.')
prov <- unique(countryShp$NAME_1)

for (i in 28:length(prov)){
  geoData_AOI <- readGeo_CM(country="Kenya",  useCaseName = "KALRO", Crop = "Maize", AOI = TRUE, season=1, Province = prov[i])}

