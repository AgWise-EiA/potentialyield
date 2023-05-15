

#################################################################################################################
## source "get_rain_temp_summary.R" function and get rain and temperature summary data 
#################################################################################################################
source("~/agwise/AgWise_Scripts/data_sourcing/get_geoSpatialTemperature_MC.R")



#################################################################################################################
## get daily TMax and Tmin
## for trial sites from AgEra
#################################################################################################################
trial_point_Tmax_AgEra <- get_temp_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE, overwrite = TRUE,
                                             Planting_month_date = NULL,  harvest_month_date = NULL, 
                                             planting_harvest_sameYear = NULL, jobs=10, season=NULL, dataSource = "AgEra", varName = "Tmax")


trial_point_Tmin_AgEra <- get_temp_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE, overwrite = TRUE,
                                              Planting_month_date = NULL,  harvest_month_date = NULL, 
                                              planting_harvest_sameYear = NULL, jobs=10, season=NULL, dataSource = "AgEra", varName = "Tmin")

#################################################################################################################
## get daily rainfall 
## for trial sites from AgEra5 :: 
# TODO test when the AgEra rainfall .nc layers are made available on CG Labs
#################################################################################################################

trial_point_Rf_AgEra5 <- get_rf_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE,
                                   overwrite = TRUE,season=NULL, Planting_month_date = NULL,  
                                   harvest_month_date = NULL, planting_harvest_sameYear = NULL, jobs=10, dataSource = "AgEra")


trial_point_Tmax_AgEra <- get_temp_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE, overwrite = TRUE,
                                             Planting_month_date = NULL,  harvest_month_date = NULL, 
                                             planting_harvest_sameYear = NULL, jobs=10, season=NULL, dataSource = "AgEra", varName = "Tmax")

              

#################################################################################################################
## get Tmin and Tmax summaries
## for AOI from AgEra: when planting and harvest year are the same
## TODO the same can be done for chirts when the layers are available by adjusting the dataSource
#################################################################################################################

AOI_Tmax_summary_sameYear_p1 <- get_temp_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE, overwrite = TRUE,
          season="season1", Planting_month_date = "02_05", harvest_month_date = "06_05",
          planting_harvest_sameYear = TRUE, jobs=10, varName = "Tmax", dataSource = "AgEra")


AOI_Tmin_summary_sameYear_p1 <- get_temp_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato",
                                                          AOI = TRUE, overwrite = TRUE, season="season1", 
                                                          Planting_month_date = "02_05", harvest_month_date = "06_05",
                                                          planting_harvest_sameYear = TRUE, jobs=10, varName = "Tmin", 
                                                          dataSource = "AgEra")





#################################################################################################################
## get daily rainfall 
## for AOI from CHIRPS: when planting and harvest year are different
## TODO the same can be done for AgEra when the layers are available by adjusting the dataSource
#################################################################################################################

AOI_point_Rf_diffYears_p6_s2  <- get_rf_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE,
                                  overwrite = TRUE,season="season2", Planting_month_date = "09-02",  
                                  harvest_month_date = "01-02", planting_harvest_sameYear = FALSE, jobs=10, dataSource = "CHIRPS")


AOI_point_Rf_diffYears_p6_s2  <- get_rf_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE,
                                                  overwrite = TRUE,season="season2", Planting_month_date = "09-09",  
                                                  harvest_month_date = "01-09", planting_harvest_sameYear = FALSE, jobs=10, dataSource = "CHIRPS")



#################################################################################################################
## get rainfall summaries (total RF, monthly rf and nr of raint days)
## for trial locations from CHIRPS
#################################################################################################################

### season 1 planting from early Feb to end march
Rain_summary_trialLoc <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE, 
                                                 overwrite = TRUE, season="NULL", Planting_month_date = NULL,
                                                 harvest_month_date = NULL, planting_harvest_sameYear = NULL, 
                                                 jobs=10, dataSource = "CHIRPS")


#################################################################################################################
## get rainfall summaries (total RF, monthly rf and nr of raint days) 
## for AOI from CHIRPS: when planting and harvest year are different for for 9 planting dates in season 1 planting from early Feb to end march
## TODO the same can be done for AgEra when the layers are available by adjusting the dataSource
#################################################################################################################


Rain_AOI_summary_s1_p1 <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE, overwrite = TRUE,
                                  season="season1", Planting_month_date = "02-05",  harvest_month_date = "06-05",
                                  planting_harvest_sameYear = TRUE, jobs=10, dataSource = "CHIRPS")


Rain_AOI_summary_s1_p2 <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE, overwrite = TRUE,
                                          season="season1", Planting_month_date = "02-12",  harvest_month_date = "06-12",
                                          planting_harvest_sameYear = TRUE, jobs=10)


Rain_AOI_summary_s1_p3 <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE, overwrite = TRUE,
                                                season="season1", Planting_month_date = "02-19",  harvest_month_date = "06-19",
                                                planting_harvest_sameYear = TRUE, jobs=10)


Rain_AOI_summary_s1_p4 <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE, overwrite = TRUE,
                                                season="season1", Planting_month_date = "02-26",  harvest_month_date = "06-26",
                                                planting_harvest_sameYear = TRUE, jobs=10)


Rain_AOI_summary_s1_p5 <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE, overwrite = TRUE,
                                                season="season1", Planting_month_date = "03-05",  harvest_month_date = "07-05",
                                                planting_harvest_sameYear = TRUE, jobs=10)


Rain_AOI_summary_s1_p6 <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE, overwrite = TRUE,
                                                season="season1", Planting_month_date = "03-12",  harvest_month_date = "07-12",
                                                planting_harvest_sameYear = TRUE, jobs=10)

Rain_AOI_summary_s1_p7 <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE, overwrite = TRUE,
                                                season="season1", Planting_month_date = "03-19",  harvest_month_date = "07-19",
                                                planting_harvest_sameYear = TRUE, jobs=10)

Rain_AOI_summary_s1_p8 <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE, overwrite = TRUE,
                                                season="season1", Planting_month_date = "03-26",  harvest_month_date = "06-26",
                                                planting_harvest_sameYear = TRUE, jobs=10)


#################################################################################################################
## get rainfall summaries (total RF, monthly rf and nr of raint days) 
## for AOI from CHIRPS: when planting and harvest year are different for 9 planting dates in season 2 planting from early Aug to end Sep
## TODO the same can be done for AgEra when the layers are available by adjusting the dataSource
#################################################################################################################



Rain_AOI_summary_s2_p1 <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE, overwrite = TRUE,
                                                season="season2", Planting_month_date = "08-05",  harvest_month_date = "12-05",
                                                planting_harvest_sameYear = TRUE, jobs=10)

Rain_AOI_summary_s2_p2 <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE, overwrite = TRUE,
                                                season="season2", Planting_month_date = "08-12",  harvest_month_date = "12-12",
                                                planting_harvest_sameYear = TRUE, jobs=10)

Rain_AOI_summary_s2_p3 <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE, overwrite = TRUE,
                                                season="season2", Planting_month_date = "08-19",  harvest_month_date = "12-19",
                                                planting_harvest_sameYear = TRUE, jobs=10)

Rain_AOI_summary_s2_p4 <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE, overwrite = TRUE,
                                                season="season2", Planting_month_date = "08-26",  harvest_month_date = "12-26",
                                                planting_harvest_sameYear = TRUE, jobs=10)

Rain_AOI_summary_s2_p5 <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE, overwrite = TRUE,
                                                season="season2", Planting_month_date = "09-02",  harvest_month_date = "01-02",
                                                planting_harvest_sameYear = FALSE, jobs=10)

Rain_AOI_summary_s2_p6 <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE, overwrite = TRUE,
                                                season="season2", Planting_month_date = "09-09",  harvest_month_date = "01-09",
                                                planting_harvest_sameYear = FALSE, jobs=10)

Rain_AOI_summary_s2_p7 <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE, overwrite = TRUE,
                                                season="season2", Planting_month_date = "09-16",  harvest_month_date = "01-16",
                                                planting_harvest_sameYear = FALSE, jobs=10)

Rain_AOI_summary_s2_p8 <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE, overwrite = TRUE,
                                                season="season2", Planting_month_date = "09-23",  harvest_month_date = "01-23",
                                                planting_harvest_sameYear = FALSE, jobs=10)

Rain_AOI_summary_s2_p9 <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE, overwrite = TRUE,
                                                season="season2", Planting_month_date = "09-30",  harvest_month_date = "01-30",
                                                planting_harvest_sameYear = FALSE, jobs=10)














