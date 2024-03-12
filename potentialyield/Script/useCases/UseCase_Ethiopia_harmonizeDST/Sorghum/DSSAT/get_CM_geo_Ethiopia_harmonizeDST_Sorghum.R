

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
country <- "Ethiopia"
useCaseName <- "harmonizeDST"
Crop <- "Sorghum"

inputDataZ <- readRDS("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Ethiopia_harmonizeDST/Sorghum/raw/sorghum_AOI.RDS")

for( zones in unique(inputDataZ$Zone)){
  print(zones)
  geoData_AOI <- readGeo_CM_zone(country=country,  useCaseName = useCaseName , Crop = Crop, AOI = TRUE, season=1, zone = zones)
}
