

##################################################################################################################
## source "get_rain_temp_summary.R" function and get weather data 
##################################################################################################################
# source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/readGeo_CM_zone.R")

# source("~/transform-modelling/AgWise_Siya/readGeo_CM_zone_SM.R") #local copy
source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/readGeo_CM_zone.R") #Server copy

#################################################################################################################
## Create soil and weather data in DSSAT format for trial data
#################################################################################################################

#geoData <- readGeo_CM(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Soybean", AOI = FALSE,  Planting_month_date = NULL,Harvest_month_date=NULL,season=NULL,plantingWindow=NULL)

#################################################################################################################
## Create soil and weather data in DSSAT format for AOI data
#################################################################################################################
country <- "Zambia"
useCaseName <- "Solidaridad"
Crop <- "Maize"

inputDataZ <- readRDS("~/agwise-datacuration/dataops/datacuration/Data/useCase_Zambia_Solidaridad/Maize/result/AOI_GPS.RDS")

provinces <-  c('North-Western','Western')
variety <- "999991"

# provinces <-  c('Lusaka','Copperbelt', 'Eastern', 'Southern', 'Muchinga', 'Central', 'North-Western', 'Western')

mz <- which(inputDataZ$NAME_1 %in% provinces)

inputDataZ <-  inputDataZ[mz,]





for (z in 1:length(provinces)){
  district<-unique(inputDataZ$NAME_2[inputDataZ$NAME_1==provinces[z]])
  for (i in 1:length(district)){
     print(provinces[z])
     print(district[i])
  geoData_AOI <- readGeo_CM_zone(country=country,  useCaseName = useCaseName , Crop = Crop, AOI = TRUE, season=1, zone = provinces[z],level2=district[i],variety=variety)
  }
}





