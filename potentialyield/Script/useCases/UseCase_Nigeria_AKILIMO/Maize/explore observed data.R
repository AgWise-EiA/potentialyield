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


#soildata <- readRDS("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_Nigeria_AKILIMO/Maize/raw/geo_4cropModel/SoilDEM_PointData_trial_profile.RDS")
#temp_data <- readRDS("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_Nigeria_AKILIMO/Maize/raw/geo_4cropModel/temperatureMax_PointData_trial.RDS")

#test <- merge(maize_nigeria,soildata)

simulated_data <- readRDS("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_Nigeria_AKILIMO/Maize/result/DSSAT/useCase_Nigeria_AKILIMO_Maize.rds")
simulated_data$lon <- simulated_data$LONG
simulated_data$lat <- simulated_data$XLAT
simulated_data$plantingDate <- as.Date(simulated_data$PDAT)

maize_nigeria$lon <- round(maize_nigeria$lon, digits=3)
maize_nigeria$lat <- round(maize_nigeria$lat, digits=3)

sim_obs <- merge(simulated_data, maize_nigeria)


ggplot(sim_obs, aes(x=yield, y=HWAH, colour=TNAM)) +
  geom_point()+facet_wrap(~month(plantingDate))


sim_obs$sim_duration <- sim_obs$HDAT-sim_obs$PDAT

ggplot(sim_obs[sim_obs$sim_duration>0,], aes(x=duration, y=sim_duration, colour=TNAM)) +
  geom_point()+facet_wrap(~TNAM)


ggplot(sim_obs, aes(x=yield, y=HWAH, colour=TNAM)) +
  geom_point()+facet_wrap(~TNAM)

sim_obs <-sim_obs[, -26, drop = FALSE]

sim_obs$group <- as.factor(paste0(sim_obs$lon, sim_obs$lat, sim_obs$plantingDate, sim_obs$harvestDate, sim_obs$TNAM))

sim_obs <- sim_obs[!sim_obs$source %in% c("ITTA-Genetic-Camila"),]
summaryall <- sim_obs %>%
  group_by(lon, lat, plantingDate, harvestDate, TNAM) %>%
  dplyr::summarise(
    max_obs_yield = max(yield, na.rm = TRUE),
   # sd_obs_yield = sd(yield, na.rm = TRUE),
    mean_sim_yield = mean(HWAH, na.rm = TRUE),
    sd_sim_yield = sd(HWAH, na.rm = TRUE)
  )




ggplot(summaryall,aes(x=max_obs_yield, y=mean_sim_yield, color=TNAM)) + 
  geom_point()+facet_wrap(~TNAM)+
  geom_abline(slope=1)+geom_abline(intercept=1000, slope=1)+geom_abline(intercept=-1000, slope=1)
 # geom_errorbar(aes(xmin=mean_obs_yield-sd_obs_yield, xmax=mean_obs_yield+sd_obs_yield), width=.8,
  #              position=position_dodge(0.05))
 