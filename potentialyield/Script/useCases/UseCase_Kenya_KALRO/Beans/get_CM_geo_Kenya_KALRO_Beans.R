

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
country <- "Kenya"
useCaseName <- "KALRO"
Crop <- "Beans"

inputDataZ <- readRDS(paste0("~/agwise-datacuration/dataops/datacuration/Data/useCase_",country,"_",useCaseName,"/", Crop, "/result/AOI_GPS.RDS"))

for(zones in unique(inputDataZ$NAME_1)){
  print(zones)
  geoData_AOI <- readGeo_CM_zone(country=country,  useCaseName = useCaseName , Crop = Crop, AOI = TRUE, season=1, zone = zones)
}
