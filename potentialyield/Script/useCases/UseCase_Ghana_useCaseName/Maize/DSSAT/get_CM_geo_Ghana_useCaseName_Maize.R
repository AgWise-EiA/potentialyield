


#################################################################################################################
## source "get_rain_temp_summary.R" function and get weather data 
#################################################################################################################
source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/Testing/readGeo_CM_zone.R")

#################################################################################################################
## Create soil and weather data in DSSAT format for trial data
#################################################################################################################

country <- "Ghana"
countryShp <- geodata::gadm(country, level = 1, path='.')
prov <- unique(countryShp$NAME_1)

for (i in 1:length(prov)){
  geoData_AOI <- readGeo_CM(country="Ghana",  useCaseName = "useCaseName", Crop = "Maize", AOI = FALSE, season=1, Province = prov[i])}



# geoData <- readGeo_CM(country = "Zambia",  useCaseName = "Elnino", Crop = "Maize", AOI = TRUEE,  Planting_month_date = NULL,Harvest_month_date=NULL,season=NULL,plantingWindow=NULL)
# geoData <- readGeo_CM(country = "Ghana",  useCaseName = "GAIP", Crop = "Maize", AOI = FALSE,  season=1) #Initial version
# geoData <- readGeo_CM(country = "Ghana",  useCaseName = "GAIP", Crop = "Maize", AOI = FALSE,  season=1, Province = FALSE)

#################################################################################################################
## Create soil and weather data in DSSAT format for AOI data
#################################################################################################################

# geoData_AOI <- readGeo_CM(country="Zambia",  useCaseName = "Elnino", Crop = "Maize", AOI = TRUE, Planting_month_date="11-01", Harvest_month_date="03-31", season=1, plantingWindow=16)









#testing
source("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/Testing/readGeo_CM.R", echo=TRUE)
#################################################################################################################
## Create soil and weather data in DSSAT format for AOI data
#################################################################################################################
country <- "Ghana"
useCaseName <- "useCaseName"
Crop <- "Maize"
inputDataZ <- readRDS("~/agwise-datacuration/dataops/datacuration/Data/useCase_Ghana_useCaseName/Maize/result/AOI_GPS.RDS")
provinces <- which(!unique(inputDataZ$NAME_1) %in% c('Northern','Luapula'))
provinces <- unique(inputDataZ$NAME_1)[provinces]
mz <- which(inputDataZ$NAME_1 %in% provinces)
inputDataZ <-  inputDataZ[mz,]
colnames(inputDataZ)[2] <- 'Zone'
for (z in 1:length(provinces)){
  print(provinces[z])
  geoData_AOI <- readGeo_CM(country=country,  useCaseName = useCaseName , Crop = Crop, AOI = TRUE, season=1, Province = prov[i])
}

