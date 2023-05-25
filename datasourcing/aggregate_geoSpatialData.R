


## Except for soil and topography data, for AOI, both the daily data to be used for crop models and the summaries to be used for ML, are by planting and harvest dates
## at this point the summaries are computed by year for the growing season. When the dry, wet and average years are implemented we could aggregate across years matching the three classes
## for trial sites it is using actual planting and harvest dates of every trial.

#' Title
#'
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param Planting_month_date is needed only for AOI and should be Rain_monthly provided as month_date, for trial locations the actual planting date is be used so no need to change the default value
#' @param dataSource is among c("CHIRPS", "AgEra")
#' @param ID only when AOI  = FALSE, it is the column name Identifying the trial ID in compiled_fieldData.RDS
#'
#'
#' @return
#' @export
#'
#' @examples join_geospatial_4ML(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", AOI = TRUE , 
#' dataSource = "CHIRPS", Planting_month_date = "02_05", ID = "TLID")

join_geospatial_4ML <- function(country, useCaseName, Crop, AOI, Planting_month_date, dataSource,  ID){
  
  
  ## create directories to save output 
  pathOut1 <-  pathOut <- paste("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", country, "_",useCaseName, "/", Crop, "/result/", sep="")
  pathOut2 <- paste("/home/jovyan/agwise/AgWise_Data/fieldData_analytics/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/field_geoSpatial", sep="")
  pathOut3 <- paste("/home/jovyan/agwise/AgWise_Data/response_functions/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/field_geoSpatial", sep="")
  
  
  if (!dir.exists(pathOut1)){
    dir.create(file.path(pathOut1), recursive = TRUE)
  }
  
  if (!dir.exists(pathOut2)){
    dir.create(file.path(pathOut2), recursive = TRUE)
  }
  
  if (!dir.exists(pathOut3)){
    dir.create(file.path(pathOut3), recursive = TRUE)
  }
  
  
  
  if (AOI == FALSE){
    ## trial sites info
    GPS_Data <- readRDS(paste("~/agwise/AgWise_Data/fieldData_analytics/UseCase_",country, "_",useCaseName, "/", Crop, "/result/compiled_fieldData.RDS", sep=""))  
    GPS_Data$Yield <- GPS_Data$TY
    GPS_Data <- subset(GPS_Data, select=-c(TY))
    ## geo spatial point data with no time dimension
    Topography_PointData <- readRDS(paste("~/agwise/AgWise_Data/data_sourcing/UseCase_",country, "_",useCaseName, "/", Crop, "/result/Topography/Topography_PointData_trial.RDS", sep=""))
    Soil_PointData <- readRDS(paste("~/agwise/AgWise_Data/data_sourcing/UseCase_",country, "_",useCaseName, "/", Crop, "/result/Soil/Soil_PointData_trial.RDS", sep=""))
    ## daily geo spatial point data 
    if(dataSource == "CHIRPS"){
      Rainfall_summaries <- readRDS(paste("~/agwise/AgWise_Data/data_sourcing/UseCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Rainfall_summaries_trial_CHIRPS.RDS", sep=""))
    }else{
      Rainfall_summaries <- readRDS(paste("~/agwise/AgWise_Data/data_sourcing/UseCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Rainfall_summaries_trial_AgEra.RDS", sep=""))
    }
    WindSpeed_summaries <- readRDS(paste("~/agwise/AgWise_Data/data_sourcing/UseCase_",country, "_",useCaseName, "/", Crop, "/result/WindSpeed/WindSpeed_summaries_trial_AgEra.RDS", sep=""))
    Tmax_summaries <- readRDS(paste("~/agwise/AgWise_Data/data_sourcing/UseCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Tmax_summaries_trial_AgEra.RDS", sep=""))
    Tmin_summaries <- readRDS(paste("~/agwise/AgWise_Data/data_sourcing/UseCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Tmin_summaries_trial_AgEra.RDS", sep=""))
    SolarRadiation_summaries <- readRDS(paste("~/agwise/AgWise_Data/data_sourcing/UseCase_",country, "_",useCaseName, "/", Crop, "/result/SolarRadiation/SolarRadiation_summaries_trial_AgEra.RDS", sep=""))
    RelativeHumidity_summaries <- readRDS(paste("~/agwise/AgWise_Data/data_sourcing/UseCase_",country, "_",useCaseName, "/", Crop, "/result/RelativeHumidity/RelativeHumidity_summaries_trial_AgEra.RDS", sep=""))
    
    ## given some trials have 5 months growing period and some only 4, for some trials the fifth moght data is NA and ML will exclide this data
    ## to avoid this the value of the 5th months is added to the 4th month and th fifth month columns are removed. 
    Rainfall_summaries$Rain_month4 <- ifelse(!is.na(Rainfall_summaries$Rain_month5),
                                             Rainfall_summaries$Rain_month4 + Rainfall_summaries$Rain_month5, 
                                             Rainfall_summaries$Rain_month4 )
    
    WindSpeed_summaries$WindSpeed_month4 <- ifelse(!is.na(WindSpeed_summaries$WindSpeed_month5), 
                                                   WindSpeed_summaries$WindSpeed_month4 + WindSpeed_summaries$WindSpeed_month5,
                                                   WindSpeed_summaries$WindSpeed_month4 )
    
    Tmax_summaries$Tmax_month4 <- ifelse(!is.na(Tmax_summaries$Tmax_month5), 
                                         Tmax_summaries$Tmax_month4 + Tmax_summaries$Tmax_month5, Tmax_summaries$Tmax_month4 )
    
    Tmin_summaries$Tmin_month4 <- ifelse(!is.na(Tmin_summaries$Tmin_month5), 
                                         Tmin_summaries$Tmin_month4 + Tmin_summaries$Tmin_month5, Tmin_summaries$Tmin_month4 )
    
    RelativeHumidity_summaries$RH_month4 <- ifelse(!is.na(RelativeHumidity_summaries$RH_month5), 
                                                   RelativeHumidity_summaries$RH_month4 + RelativeHumidity_summaries$RH_month5, 
                                                   RelativeHumidity_summaries$RH_month4 )
    
    
    ## merge data 
    Rainfall_summaries <- subset(Rainfall_summaries, select=-c(longitude, latitude, plantingDate, harvestDate, NAME_1, NAME_2, Rain_month5))
    Tmax_summaries <- subset(Tmax_summaries, select=-c(longitude, latitude, plantingDate, harvestDate,NAME_1, NAME_2, Tmax_month5))
    Tmin_summaries <- subset(Tmin_summaries, select=-c(longitude, latitude, plantingDate, harvestDate, NAME_1, NAME_2, Tmin_month5))
    RelativeHumidity_summaries <- subset(RelativeHumidity_summaries, select=-c(longitude, latitude, plantingDate, harvestDate,NAME_1, NAME_2, RH_month5))
    WindSpeed_summaries <- subset(WindSpeed_summaries, select=-c(longitude, latitude, plantingDate, harvestDate,NAME_1, NAME_2, WindSpeed_month5))
    SolarRadiation_summaries <- subset(SolarRadiation_summaries, select=-c(longitude, latitude, plantingDate, harvestDate, solarRad_month5))
    Topography_PointData <- subset(Topography_PointData, select=-c(longitude, latitude))
    Soil_PointData <- subset(Soil_PointData, select=-c(longitude, latitude))
    
   
    df_list1 <- list(GPS_Data, Rainfall_summaries, Tmax_summaries, Tmin_summaries, RelativeHumidity_summaries, SolarRadiation_summaries)
    merged_df1 <- Reduce(function(x, y) merge(x, y, by = ID), df_list1)
    head(merged_df1)
    
    df_list2 <- list(merged_df1, Topography_PointData, Soil_PointData)
    merged_df2 <- Reduce(function(x, y) merge(x, y, by.x = ID , by.y = "ID"), df_list2)
 
   
  }else{
    
    ## geo-spatial point data with no time dimension
    Topography_PointData <- readRDS(paste("~/agwise/AgWise_Data/data_sourcing/UseCase_",country, "_",useCaseName, "/", Crop, "/result/Topography/Topography_PointData_AOI.RDS", sep=""))
    Soil_PointData <- readRDS(paste("~/agwise/AgWise_Data/data_sourcing/UseCase_",country, "_",useCaseName, "/", Crop, "/result/Soil/Soil_PointData_AOI.RDS", sep=""))
    
    ## daily geo spatial point data 
    if(dataSource == "CHIRPS"){
      Rainfall_summaries <- readRDS(paste("~/agwise/AgWise_Data/data_sourcing/UseCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Rainfall_summaries_AOI_", Planting_month_date, "_CHIRPS.RDS", sep=""))
    }else{
      Rainfall_summaries <- readRDS(paste("~/agwise/AgWise_Data/data_sourcing/UseCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Rainfall_summaries_AOI_",Planting_month_date, "_AgEra.RDS", sep=""))
    }
    Tmax_summaries <- readRDS(paste("~/agwise/AgWise_Data/data_sourcing/UseCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Tmax_summaries_AOI_", Planting_month_date, "_AgEra.RDS", sep=""))
    Tmin_summaries <- readRDS(paste("~/agwise/AgWise_Data/data_sourcing/UseCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Tmin_summaries_AOI_", Planting_month_date, "_AgEra.RDS", sep=""))
    SolarRadiation_summaries <- readRDS(paste("~/agwise/AgWise_Data/data_sourcing/UseCase_",country, "_",useCaseName, "/", Crop, "/result/SolarRadiation/SolarRadiation_summaries_AOI_", Planting_month_date, "_AgEra.RDS", sep=""))
    RelativeHumidity_summaries <- readRDS(paste("~/agwise/AgWise_Data/data_sourcing/UseCase_",country, "_",useCaseName, "/", Crop, "/result/RelativeHumidity/RelativeHumidity_summaries_AOI_",Planting_month_date, "_AgEra.RDS", sep=""))
  
    ## merging data
    Meta_summaries <- unique(Rainfall_summaries[, c("Planting", "Harvesting", "plantingDate", "harvestDate")])
    Rainfall_summaries <- subset(Rainfall_summaries, select=-c(Planting, Harvesting, harvestYear, plantingDate, harvestDate))
    Tmax_summaries <- subset(Tmax_summaries, select=-c(Planting, Harvesting, harvestYear, plantingDate, harvestDate))
    Tmin_summaries <- subset(Tmin_summaries, select=-c(Planting, Harvesting, harvestYear, plantingDate, harvestDate))
    RelativeHumidity_summaries <- subset(RelativeHumidity_summaries, select=-c(Planting, Harvesting, harvestYear, plantingDate, harvestDate))
    SolarRadiation_summaries <- subset(SolarRadiation_summaries, select=-c(Planting, Harvesting, harvestYear, plantingDate, harvestDate))
    
    
    df_list1 <- list(Rainfall_summaries, Tmax_summaries, Tmin_summaries, RelativeHumidity_summaries, SolarRadiation_summaries)
    merged_df1 <- Reduce(function(x, y) merge(x, y, by = c("longitude", "latitude", "plantingYear", "NAME_1", "NAME_2"), all = TRUE), df_list1)
    head(merged_df1)
    
    df_list2 <- list(merged_df1, Topography_PointData, Soil_PointData)
    merged_df2 <- Reduce(function(x, y) merge(x, y, by = c("longitude", "latitude", "NAME_1", "NAME_2"), all = TRUE), df_list2)
   
    
    merged_df2 <- merged_df2[!merged_df2$plantingYear %in% c(1979, 1980, 2023), ]
    merged_df2 <- merged_df2[order(merged_df2$plantingYear), ]
   
  }
  
  merged_df2 <- merged_df2[complete.cases(merged_df2), ]
  
  fname <- ifelse(AOI == TRUE, "linked_geoSpatial_AOI.RDS", "linked_geoSpatial_trial.RDS")
  
  saveRDS(object = merged_df2, file=paste(pathOut1, fname , sep=""))
  saveRDS(object = merged_df2, file=paste(pathOut2, fname, sep=""))
  saveRDS(object = merged_df2, file=paste(pathOut3, fname, sep=""))
  
  return(merged_df2)
  
  
}








