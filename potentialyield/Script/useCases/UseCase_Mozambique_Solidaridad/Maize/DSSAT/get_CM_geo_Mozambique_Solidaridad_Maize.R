

#################################################################################################################
## source "get_rain_temp_summary.R" function and get weather data 
#################################################################################################################
#source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/readGeo_CM_V2.R")

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/readGeo_CM_V3.R")

#################################################################################################################
## Create soil and weather data in DSSAT format for trial data
#################################################################################################################

#geoData <- readGeo_CM(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Soybean", AOI = FALSE,  Planting_month_date = NULL,Harvest_month_date=NULL,season=NULL,plantingWindow=NULL)

#################################################################################################################
## Create soil and weather data in DSSAT format for AOI data
#################################################################################################################
country <- "Mozambique"
countryShp <- geodata::gadm(country, level = 1, path='.')
prov <- unique(countryShp$NAME_1)

#prov <- c("Tete","Nampula", "Cabo Delgado", "Zambezia", "Manica") #"Manica"

prov <- c("Tete", "Zambezia", "Nampula", "Cabo Delgado", "Manica")

Planting_month_date <- "11-01" ## the earliest possible plating mm-dd

Harvest_month_date <- "07-30" ## the earliest harvest date in mm-dd (https://www.apni.net/wp-content/uploads/2022/05/4R-Maize-Guide-0511.pdf)


for (i in 1:length(prov)){
  geoData_AOI <- readGeo_CM(country="Mozambique",  useCaseName = "Solidaridad", Crop = "Maize", AOI = TRUE, season=1, Province = prov[i])
  }

