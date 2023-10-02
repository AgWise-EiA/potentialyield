library("RColorBrewer")
maize_nigeria <- readRDS("/home/jovyan/agwise-datacuration/dataops/datacuration/Data/useCase_Nigeria_AKILIMO/Maize/result/compiled_fieldData.RDS")
maize_nigeria$plantingDate <- as.Date(maize_nigeria$plantingDate)
maize_nigeria$harvestDate <- as.Date(maize_nigeria$harvestDate)
maize_nigeria$duration <- as.numeric(maize_nigeria$harvestDate - maize_nigeria$plantingDate)
ggplot(maize_nigeria, aes(x=plantingDate, y=yield, color=source)) +
  geom_point()

maize_nigeria$year <- year(maize_nigeria$plantingDate)
ggplot(maize_nigeria, aes(x=as.factor(year), y=yield, fill=source)) +
  geom_boxplot()

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
ggplot(maize_nigeria, aes(x=plantingDate, y=yield, colour=duration)) +
  geom_point()+ scale_colour_gradientn(colours = myPalette(100))


ggplot(maize_nigeria, aes(x = duration)) +
  geom_histogram() 

soildata <- readRDS("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_Nigeria_AKILIMO/Maize/raw/geo_4cropModel/SoilDEM_PointData_trial_profile.RDS")
temp_data <- readRDS("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_Nigeria_AKILIMO/Maize/raw/geo_4cropModel/temperatureMax_PointData_trial.RDS")

test <- merge(maize_nigeria,soildata)
