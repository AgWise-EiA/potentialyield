source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/Testing/readGeo_CM_V2.R")

#################################################################################################################
## Create soil and weather data in DSSAT format for trial data
#################################################################################################################

#geoData <- readGeo_CM(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Soybean", AOI = FALSE,  Planting_month_date = NULL,Harvest_month_date=NULL,season=NULL,plantingWindow=NULL)

#################################################################################################################
## Create soil and weather data in DSSAT format for AOI data
#################################################################################################################
country <- "Nigeria"
countryShp <- geodata::gadm(country, level = 2, path='.')
prov <- unique(countryShp$NAME_1)

for (i in 1:length(prov)){
  geoData_AOI <- readGeo_CM(country="Nigeria",  useCaseName = "SSA", Crop = "Maize", AOI = TRUE, season=1, Province = prov[i])}



