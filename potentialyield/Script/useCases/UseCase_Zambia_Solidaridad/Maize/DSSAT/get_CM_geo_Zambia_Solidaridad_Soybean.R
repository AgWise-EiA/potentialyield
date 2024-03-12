

#################################################################################################################
## source "get_rain_temp_summary.R" function and get weather data 
#################################################################################################################
#source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/readGeo_CM_zone.R")
source("~/Agwise/potentialyield/scripts/generic/readGeo_CM_zone.R", echo=TRUE)

#################################################################################################################
## Create soil and weather data in DSSAT format for trial data
#################################################################################################################

#geoData <- readGeo_CM(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Soybean", AOI = FALSE,  Planting_month_date = NULL,Harvest_month_date=NULL,season=NULL,plantingWindow=NULL)

#################################################################################################################
## Create soil and weather data in DSSAT format for AOI data
#################################################################################################################
country <- "Zambia"
useCaseName <- "Solidaridad"
Crop <- "Soybean"

inputDataZ <- readRDS("~/agwise-datacuration/dataops/datacuration/Data/useCase_Zambia_Solidaridad/Maize/result/AOI_GPS.RDS")
provinces <- which(!unique(inputDataZ$NAME_1) %in% c('Northern','Luapula'))
provinces <- unique(inputDataZ$NAME_1)[provinces]

provinces <-  c('Central', "")

#provinces <-  c('Lusaka','Copperbelt', 'Eastern', 'Southern', 'Muchinga', 'Central', 'North-Western', 'Western')

mz <- which(inputDataZ$NAME_1 %in% provinces)

inputDataZ <-  inputDataZ[mz,]

colnames(inputDataZ)[2] <- 'Zone'

for (z in 1:length(provinces)){
  print(provinces[z])
  geoData_AOI <- readGeo_CM_zone(country=country,  useCaseName = useCaseName , Crop = Crop, AOI = TRUE, season=1, zone = provinces[z])
}