
#################################################################################################################
## 1.  Sourcing the geo-spatial soil and weather data for Rwanda Maize growing areas. The data is sourced apart for the experimental sites and for the area of interest (AOI) for scaling apart. 
#################################################################################################################


source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialData.R")
## Data for crop models at trial sites : weather + the 6 profiels of soil grids soil data
RAB_Potato_weather_soil_trial_profile <- extract_geoSpatialPointData(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", 
                                                                     AOI=FALSE, Planting_month_date=NULL, Harvest_month_date=NULL, 
                                                                     soilData = FALSE, weatherData = TRUE, soilProfile = TRUE, season = 1, 
                                                                     jobs =10)
## Data for crop models at AOI : weather + the 6 profiels of soil grids soil data
RAB_potato_weather_soil_AOI_profileS1 <- extract_geoSpatialPointData(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", 
                                                                     AOI=TRUE, Planting_month_date = "08-08",  Harvest_month_date = "12-08",
                                                                     soilData = TRUE, weatherData = TRUE, soilProfile = TRUE, plantingWindow = 4, 
                                                                     season = 1, jobs =10)

#################################################################################################################
## 2. Create soil and weather data in DSSAT format for trial data and AOI
#################################################################################################################
source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/readGeo_CM.R")
geoData <- readGeo_CM(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE, season=NULL)
geoData_AOI <- readGeo_CM(country="Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE, season=1)


TODO .....
#################################################################################################################
## 3. Create experimental data in DSSAT format
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/dssat_expfile.R")
expdata <- dssat.expfile(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE, filex_temp="MZRM8143.MZX",
                         Planting_month_date = NULL, Harvest_month_date=NULL, jobs=10, ID="TLID",season =NULL, plantingWindow=NULL)

expdata_AOI <- dssat.expfile(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE, filex_temp="MZRM8143.MZX", Planting_month_date="08-15", Harvest_month_date="03-31",jobs=10, ID="TLID",season =1, plantingWindow=4)

#################################################################################################################
## 4. Run the DSSAT model
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/dssat_exec.R")
execmodel <-dssat.exec(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE,TRT=1, cultivarType = "mediumDuration")
execmodel_AOI <-dssat.exec(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE,TRT=1:5)

#################################################################################################################
## 5. Merge results
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/merge_DSSAT_output.R")
mergeresults <-merge_DSSAT_output(country = "Rwanda",  useCaseName = "RAB", Crop = "Maize", AOI = FALSE,season=NULL)
mergeresults_AOI <-merge_DSSAT_output(country = "Rwanda",  useCaseName = "RAB", Crop = "Maize", AOI = TRUE,season=1)







