

#################################################################################################################
## source "get_rain_temp_summary.R" function and get rain and temperature summary data 
#################################################################################################################
source("~/agwise/AgWise_Scripts/data_sourcing/get_geoSpatialSolarRadiation_MC.R")



#################################################################################################################
## get daily Solar Radiation
## for trial sites from AgEra5
#################################################################################################################
trial_point_SR_AgEra <- get_SolarRadiation_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE,
                                             overwrite = TRUE, Planting_month_date = NULL,  Harvest_month_date = NULL, 
                                             jobs=10,  dataSource = "AgEra", ID = "TLID")


#################################################################################################################
## get daily Solar Radiation 
## for AOI from AgEra5 :: 
#################################################################################################################

AOI_point_SR_AgEra5 <- get_SolarRadiation_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE,
                                            overwrite = TRUE, Planting_month_date = "02-05", 
                                            Harvest_month_date = "06-05", jobs=10, dataSource = "AgEra")

head(AOI_point_SR_AgEra5)

#################################################################################################################
## get Solar Radiation summaries
## for AOI from AgEra
#################################################################################################################
#TODO 
AOI_SR_summary_p1 <- get_SolarRadiation_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", 
                                                 AOI = TRUE, overwrite = TRUE, Planting_month_date = "02-05", 
                                                 Harvest_month_date = "06-05", jobs=10, dataSource = "AgEra", ID = NULL)


#################################################################################################################
## get Solar Radiation summaries
## for trial sites from AgEra
#################################################################################################################

trial_SR_summary_p1 <- get_SolarRadiation_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", 
                                                   AOI = FALSE, overwrite = TRUE, Planting_month_date = NULL, 
                                                   Harvest_month_date = NULL, jobs=10, dataSource = "AgEra", ID = "TLID")




