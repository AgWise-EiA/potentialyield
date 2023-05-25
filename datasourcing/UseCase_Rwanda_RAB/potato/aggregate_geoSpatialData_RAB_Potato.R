

#################################################################################################################
## source "get_rain_temp_summary.R" function and get rain summary data 
#################################################################################################################
source("~/agwise/AgWise_Scripts/data_sourcing/aggregate_geoSpatialData.R")


#################################################################################################################
## get daily rainfall 
## for trial sites from CHIRPS  and AgEra5 
#################################################################################################################


trial_geoSpatial_4ML <- join_geospatial_4ML(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", AOI = FALSE , 
                             dataSource = "CHIRPS", Planting_month_date = NULL, ID = "TLID")



AOI_geoSpatial_4ML <- join_geospatial_4ML(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", AOI = TRUE , 
                                            dataSource = "CHIRPS", Planting_month_date = "02_05", ID = "TLID")





