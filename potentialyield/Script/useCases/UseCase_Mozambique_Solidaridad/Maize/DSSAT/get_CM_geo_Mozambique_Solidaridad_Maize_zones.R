

#################################################################################################################
## source "get_rain_temp_summary.R" function and get weather data 
#################################################################################################################
#source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/readGeo_CM_zone.R")

source("~/Agwise/potentialyield/scripts/generic/readGeo_CM_zone.R")

#################################################################################################################
## Create soil and weather data in DSSAT format for trial data
#################################################################################################################

#geoData <- readGeo_CM(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Soybean", AOI = FALSE,  Planting_month_date = NULL,Harvest_month_date=NULL,season=NULL,plantingWindow=NULL)

#################################################################################################################
## Create soil and weather data in DSSAT format for AOI data
#################################################################################################################
country <- "Mozambique"
useCaseName <- "Solidaridad"
Crop <- "Maize"

inputDataZ <- readRDS("~/agwise-datacuration/dataops/datacuration/Data/useCase_Mozambique_Solidaridad/Maize/result/AOI_GPS.RDS")

#provinces <-  c('Nampula', 'Cabo Delgado', 'Tete', 'Zambezia', 'Manica')

provinces <-  c('Tete', 'Zambezia')

mz <- which(inputDataZ$NAME_1 %in% provinces)

inputDataZ <-  inputDataZ[mz,]

#inputDataZ <- inputDataZ[c(1:1500,nrow(inputDataZ)-1500:nrow(inputDataZ)),]

# Take the first half

colnames(inputDataZ)[2] <- 'Zone'

for (z in 1:length(provinces)){
  print(provinces[z])
  geoData_AOI <- readGeo_CM_zone(country=country,  useCaseName = useCaseName , Crop = Crop, AOI = TRUE, season=1, zone = provinces[z])
}