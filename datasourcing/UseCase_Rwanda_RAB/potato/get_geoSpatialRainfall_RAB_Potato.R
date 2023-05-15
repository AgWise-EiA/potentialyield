
#################################################################################################################
## source "get_rain_temp_summary.R" function and get rain and temperature summary data 
#################################################################################################################
source("~/agwise-datasourcing/dataops/datasourcing/get_geoSpatialRainfall.R")


#################################################################################################################
## get daily rainfall 
## for trial sites from CHIRPS (AgEra5 is not yet in CG Labs fully)
#################################################################################################################

trial_point_Rf_CHIRPS <- get_rf_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE,
              overwrite = TRUE, Planting_month_date = NULL, Harvest_month_date = NULL, jobs=10, 
              season=NULL, dataSource = "CHIRPS")


              

#################################################################################################################
## get daily rainfall 
## for AOI from CHIRPS: when planting and harvest year are the same
#################################################################################################################

AOI_point_Rf_sameYear_p1  <- get_rf_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE,
                         overwrite = TRUE,season="season1", Planting_month_date = "02-05",  
                         Harvest_month_date = "06-05", jobs=10, dataSource = "CHIRPS")


#################################################################################################################
## get rainfall summaries (total RF, monthly rf and nr of rainy days)
## for trial locations from CHIRPS
#################################################################################################################
Rain_summary_trialLoc <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE, 
                                                 overwrite = TRUE, season="NULL", Planting_month_date = NULL,
                                                 Harvest_month_date = NULL, jobs=10, dataSource = "CHIRPS")


#################################################################################################################
## get rainfall summaries (total RF, monthly rf and nr of raint days) 
## for AOI from CHIRPS
#################################################################################################################

Rain_AOI_summary_s1_p1 <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE,
                                            overwrite = TRUE, season="season1", Planting_month_date = "02-05",  
                                            Harvest_month_date = "06-05", jobs=10, dataSource = "CHIRPS")

