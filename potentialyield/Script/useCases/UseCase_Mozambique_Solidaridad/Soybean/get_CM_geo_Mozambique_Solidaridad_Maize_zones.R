

#################################################################################################################
## source "get_rain_temp_summary.R" function and get weather data 
#################################################################################################################
source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/readGeo_CM_zone.R")

#################################################################################################################
## Create soil and weather data in DSSAT format for trial data
#################################################################################################################

#geoData <- readGeo_CM(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Soybean", AOI = FALSE,  Planting_month_date = NULL,Harvest_month_date=NULL,season=NULL,plantingWindow=NULL)

#################################################################################################################
## Create soil and weather data in DSSAT format for AOI data
#################################################################################################################
country <- "Mozambique"
useCaseName <- "Solidaridad"
Crop <- "Soybean"

inputDataZ <- readRDS("~/agwise-datacuration/dataops/datacuration/Data/useCase_Mozambique_Solidaridad/Soybean/result/AOI_GPS.RDS")
# provinces <- which(!unique(inputDataZ$NAME_1) %in% c('Northern','Luapula'))
# provinces <- unique(inputDataZ$NAME_1)[provinces]

provinces <-  c('Nampula', 'Cabo Delgado', 'Tete', 'Zambezia', 'Manica')

mz <- which(inputDataZ$NAME_1 %in% provinces)

inputDataZ <-  inputDataZ[mz,]

colnames(inputDataZ)[2] <- 'Zone'

for (z in 1:length(provinces)){
  print(provinces[z])
  geoData_AOI <- readGeo_CM_zone(country=country,  useCaseName = useCaseName , Crop = Crop, AOI = TRUE, season=1, zone = provinces[z])
}
