

#################################################################################################################
## source "get_rain_temp_summary.R" function and get rain and RelativeHumidity summary data 
#################################################################################################################
source("~/agwise/AgWise_Scripts/data_sourcing/get_geoSpatialRelativeHumidity_MC.R")


#################################################################################################################
## get daily Relative Humidity
## for trial sites from AgEra5
#################################################################################################################
trial_point_RH_AgEra <- get_RelativeHumidity_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE,
                                                       overwrite = TRUE, Planting_month_date = NULL,  Harvest_month_date = NULL, 
                                                       jobs=10, dataSource = "AgEra", ID = "TLID")




#################################################################################################################
## get daily RelativeHumidity 
## for AOI from AgEra5 :: 
#################################################################################################################

AOI_point_RH_AgEra5 <- get_RelativeHumidity_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE,
                                            overwrite = TRUE, Planting_month_date = "02-05", 
                                            Harvest_month_date = "06-05", jobs=10, dataSource = "AgEra", ID = NULL)



#################################################################################################################
## get RelativeHumidity summaries
## for AOI from AgEra
#################################################################################################################

AOI_RH_summary_p1 <- get_RelativeHumidity_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", 
                                                 AOI = TRUE, overwrite = TRUE, Planting_month_date = "02-05", 
                                                 Harvest_month_date = "06-05", jobs=10, dataSource = "AgEra", ID = NULL)


#################################################################################################################
## get RelativeHumidity summaries
## for trial sites from AgEra
#################################################################################################################

trial_RH_summary_p1 <- get_RelativeHumidity_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", 
                                                   AOI = FALSE, overwrite = TRUE,  Planting_month_date = NULL, 
                                                   Harvest_month_date = NULL, jobs=10, dataSource = "AgEra", ID = "TLID")




