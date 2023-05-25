

#################################################################################################################
## source "get_rain_temp_summary.R" function and get rain and Relative Humidity summary data 
#################################################################################################################
source("~/agwise/AgWise_Scripts/data_sourcing/get_geoSpatialData_4CropModels.R")



#################################################################################################################
## get geo-spatial data for the AOI sites: data in the format crop models can use
#################################################################################################################
#1. Relative Humidity
AOI_RH_AgEra_4CM <- get_data_4CropModels(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE,
                                           overwrite = TRUE, Planting_month_date = "02-05",  Harvest_month_date = "06-05", 
                                           jobs=10, dataSource = "AgEra", ID = "TLID", varName = "RelativeHumidity")

#2. Wind Speed
AOI_WS_AgEra_4CM <- get_data_4CropModels(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE,
                                           overwrite = TRUE, Planting_month_date = "02-05",  Harvest_month_date = "06-05", 
                                           jobs=10, dataSource = "AgEra", ID = "TLID", varName = "WindSpeed")

#3.Rainfall
AOI_RF_AgEra_4CM <- get_data_4CropModels(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE,
                                           overwrite = TRUE, Planting_month_date = "02-05",  Harvest_month_date = "06-05", 
                                           jobs=10, dataSource = "AgEra", ID = "TLID", varName = "Rainfall")

#4.Solar Radiation
AOI_SR_AgEra_4CM <- get_data_4CropModels(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE,
                                           overwrite = TRUE, Planting_month_date = "02-05",  Harvest_month_date = "06-05", 
                                           jobs=10, dataSource = "AgEra", ID = "TLID", varName = "SolarRadiation")

#5.Temperature max
AOI_TempMax_AgEra_4CM <- get_data_4CropModels(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE,
                                                overwrite = TRUE, Planting_month_date = "02-05", Harvest_month_date = "06-05", 
                                                jobs=10, dataSource = "AgEra", ID = "TLID", varName = "TemperatureMax")

#6.Temperature min
AOI_TempMax_AgEra_4CM <- get_data_4CropModels(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE,
                                                overwrite = TRUE, Planting_month_date = "02-05",  Harvest_month_date = "06-05", 
                                                jobs=10, dataSource = "AgEra", ID = "TLID", varName = "TemperatureMin")




#################################################################################################################
## get geo-spatial data for the trial sites: data in the format crop models can use
#################################################################################################################
#1. RelativeHumidity
trial_RH_AgEra_4CM <- get_data_4CropModels(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE,
                                           overwrite = TRUE, Planting_month_date = NULL,  Harvest_month_date = NULL, 
                                           jobs=10, dataSource = "AgEra", ID = "TLID", varName = "RelativeHumidity")

#2. WindSpeed
trial_WS_AgEra_4CM <- get_data_4CropModels(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE,
                                           overwrite = TRUE, Planting_month_date = NULL,  Harvest_month_date = NULL, 
                                           jobs=10, dataSource = "AgEra", ID = "TLID", varName = "WindSpeed")

#3.Rainfall
trial_RF_CHIRPS_4CM <- get_data_4CropModels(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE,
                                           overwrite = TRUE, Planting_month_date = NULL,  Harvest_month_date = NULL, 
                                           jobs=10, dataSource = "CHIRPS", ID = "TLID", varName = "Rainfall")

trial_RF_AgEra_4CM <- get_data_4CropModels(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE,
                                            overwrite = TRUE, Planting_month_date = NULL,  Harvest_month_date = NULL, 
                                            jobs=10, dataSource = "AgEra", ID = "TLID", varName = "Rainfall")


#4.Solar Radiation
trial_SR_AgEra_4CM <- get_data_4CropModels(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE,
                                           overwrite = TRUE, Planting_month_date = NULL,  Harvest_month_date = NULL, 
                                           jobs=10, dataSource = "AgEra", ID = "TLID", varName = "SolarRadiation")


#5.Temperature max
trial_TempMax_AgEra_4CM <- get_data_4CropModels(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE,
                                           overwrite = TRUE, Planting_month_date = NULL,  Harvest_month_date = NULL, 
                                           jobs=10, dataSource = "AgEra", ID = "TLID", varName = "TemperatureMax")

#6.Temperature min
trial_TempMin_AgEra_4CM <- get_data_4CropModels(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE,
                                                overwrite = TRUE, Planting_month_date = NULL,  Harvest_month_date = NULL, 
                                                jobs=10, dataSource = "AgEra", ID = "TLID", varName = "TemperatureMin")




