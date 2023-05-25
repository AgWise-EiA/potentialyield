

#################################################################################################################
## source "get_rain_temp_summary.R" function and get rain and temperature summary data 
#################################################################################################################
#source("~/agwise/AgWise_Scripts/data_sourcing/get_geoSpatialWindSpeed_MC.R")
source("~/agwise-datasourcing/dataops/datasourcing/get_geoSpatialWindSpeed_MC.R")


#################################################################################################################
## get daily Wind Speed
## for trial sites from AgEra5
#################################################################################################################
trial_point_WS_AgEra <- get_WindSpeed_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE,
                                             overwrite = TRUE, Planting_month_date = NULL,  Harvest_month_date = NULL, 
                                             jobs=10, dataSource = "AgEra", ID = "TLID")


#################################################################################################################
## get daily Wind Speed 
## for AOI from AgEra5 :: 
#################################################################################################################

AOI_point_SR_AgEra5 <- get_WindSpeed_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE,
                                            overwrite = TRUE, Planting_month_date = "02-05", 
                                            Harvest_month_date = "06-05", jobs=10, dataSource = "AgEra", ID = NULL)



#################################################################################################################
## get Wind Speed summaries
## for AOI from AgEra
#################################################################################################################

AOI_WS_summary_p1 <- get_WindSpeed_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", 
                                                 AOI = TRUE, overwrite = TRUE, Planting_month_date = "02-05", 
                                                 Harvest_month_date = "06-05", jobs=10, dataSource = "AgEra", ID = NULL)


#################################################################################################################
## get Wind Speed summaries
## for trial sites from AgEra
#################################################################################################################

trial_SR_summary_p1 <- get_WindSpeed_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", 
                                                   AOI = FALSE, overwrite = TRUE, Planting_month_date = NULL, 
                                                   Harvest_month_date = NULL, jobs=10, dataSource = "AgEra", ID = "TLID")




