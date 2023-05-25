

#################################################################################################################
## source "get_rain_temp_summary.R" function and get rain summary data 
#################################################################################################################
#source("~/agwise/AgWise_Scripts/data_sourcing/get_geoSpatialRainfall.R")

source("~/agwise-datasourcing/dataops/datasourcing/get_geoSpatialRainfall.R")


#################################################################################################################
## get daily rainfall 
## for trial sites from CHIRPS  and AgEra5 
#################################################################################################################

trial_point_Rf_CHIRPS <- get_rf_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE,
              overwrite = TRUE, Planting_month_date = NULL, Harvest_month_date = NULL, jobs=10, 
              dataSource = "CHIRPS", ID = "TLID")


trial_point_Rf_AgEra <- get_rf_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE,
                                          overwrite = TRUE, Planting_month_date = NULL, Harvest_month_date = NULL, jobs=10, 
                                          dataSource = "AgEra", ID = "TLID")


#################################################################################################################
## get rainfall data ready for crop models
## for trial locations from CHIRPS and AgEra
#################################################################################################################
Rain_4CM_AOI_CHIRPS <- get_rf_4CropModels(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE,
                                          overwrite = TRUE, Planting_month_date = "02-05",  
                                          Harvest_month_date = "06-05", jobs=10, dataSource = "CHIRPS", ID = NULL)


Rain_4CM_trial_CHIRPS <- get_rf_4CropModels(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE,
                                            overwrite = TRUE, Planting_month_date = NULL, Harvest_month_date = NULL, jobs=10, 
                                            dataSource = "CHIRPS", ID = "TLID")




#################################################################################################################
## get daily rainfall 
## for AOI from CHIRPS & AgEra: when planting and harvest year are the same
#################################################################################################################

AOI_point_Rf_sameYear_p1  <- get_rf_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE,
                         overwrite = TRUE, Planting_month_date = "02-05",  
                         Harvest_month_date = "06-05", jobs=10, dataSource = "CHIRPS", ID = NULL)


AOI_point_Rf_p2  <- get_rf_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE,
                                              overwrite = TRUE, Planting_month_date = "02-05",  
                                              Harvest_month_date = "06-05", jobs=10, dataSource = "AgEra", ID = NULL)



#################################################################################################################
## get rainfall summaries (total RF, monthly rf and nr of rainy days)
## for trial locations from CHIRPS and AgEra
#################################################################################################################
Rain_summary_trialLoc_CHIRPS <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE, 
                                                 overwrite = TRUE, Planting_month_date = NULL,
                                                 Harvest_month_date = NULL, jobs=10, dataSource = "CHIRPS", ID = "TLID")


Rain_summary_trialLoc_AgEra <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE, 
                                                 overwrite = TRUE,  Planting_month_date = NULL,
                                                 Harvest_month_date = NULL, jobs=10, dataSource = "AgEra", ID = "TLID")




#################################################################################################################
## get rainfall summaries (total RF, monthly rf and nr of raint days) 
## for AOI from CHIRPS and AgEra
#################################################################################################################

Rain_AOI_summary_s1_p1_Ch <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE,
                                            overwrite = TRUE,  Planting_month_date = "02-05",  
                                            Harvest_month_date = "06-05", jobs=10, dataSource = "CHIRPS", ID = NULL)


Rain_AOI_summary_s1_p1_Ag <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE,
                                                  overwrite = TRUE, Planting_month_date = "02-05",  
                                                  Harvest_month_date = "06-05", jobs=10, dataSource = "AgEra", ID = NULL)

#################################################################################################################
## get rainfall summaries (total RF, monthly rf and nr of rainy days) 
## for Country from CHIRPS
#################################################################################################################
get_rf_rasterSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE, overwrite = TRUE,
                         Planting_month_date = "02-05",  Harvest_month_date = "06-05", jobs=10,
                         dataSource = "CHIRPS", scenario=TRUE)







