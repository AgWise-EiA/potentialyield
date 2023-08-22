b<-readRDS('/home/jovyan/agwise/testingSpace/DSSATtest/Output_data/RW_DSSAT_Maize_SHORT_S1.rds')

# b$year  <- substr(b$planting.date,1,4)
b$year  <- b$Sowing.year

b$loc <- paste0(b$Lon, ".", b$Lat)

years <- unique(b$year)

locs <- unique(b$loc)

loc.d <-  NULL

for (l in 1:length(locs)){
  bl <- subset(b, b$loc == locs[l])
  
  loc.dy <- NULL
  for (y in 1:length(years)){
    bly <- subset(bl,b$year == years[y])
    bly <- na.omit(bly)
    m <- which(bly$Yield == max(bly$Yield))
    # bly.m <- bly[m, c(1:3,6,17:18)]
    # bly.m <- bly[m, c(1:3,6,17:18)][1,]
    bly.m <- bly[m, c(1,2,3,5,8,9,19,20,25,26)][1,]
    loc.dy <- rbind(loc.dy, bly.m)
  }
  
  loc.d <- rbind(loc.d, loc.dy)
  saveRDS(loc.d, file = "/home/jovyan/agwise/testingSpace/DSSATtest/Output_data/RW_DSSAT_OptMaizeSowingDate_SHORT_S1.RDS")
  
}


#####################################################################################################################################################
# RW_DSSAT_OptMaizeSowingDate_SHORT_S1 <- readRDS("/home/jovyan/agwise/testingSpace/DSSATtest/Output_data/RW_DSSAT_OptMaizeSowingDate_SHORT_S1.RDS")
# RW_DSSAT_OptMaizeSowingDate_SHORT_S1$Country<-'Rwanda'
# RW_DSSAT_OptMaizeSowingDate_SHORT_S1$Season<-'Season 1'
# RW_DSSAT_OptMaizeSowingDate_SHORT_S1$Crop<-'Maize'
# RW_DSSAT_OptMaizeSowingDate_SHORT_S1$Variety<-'Short'
# 
# RW_DSSAT_OptMaizeSowingDate_MEDIUM_S1 <- readRDS("/home/jovyan/agwise/testingSpace/DSSATtest/Output_data/RW_DSSAT_OptMaizeSowingDate_MEDIUM_S1.RDS")
# RW_DSSAT_OptMaizeSowingDate_MEDIUM_S1$Country<-'Rwanda'
# RW_DSSAT_OptMaizeSowingDate_MEDIUM_S1$Season<-'Season 1'
# RW_DSSAT_OptMaizeSowingDate_MEDIUM_S1$Crop<-'Maize'
# RW_DSSAT_OptMaizeSowingDate_MEDIUM_S1$Variety<-'Medium'
# 
# RW_DSSAT_OptMaizeSowingDate_LONG_S1 <- readRDS("/home/jovyan/agwise/testingSpace/DSSATtest/Output_data/RW_DSSAT_OptMaizeSowingDate_LONG_S1.RDS")
# RW_DSSAT_OptMaizeSowingDate_LONG_S1$Country<-'Rwanda'
# RW_DSSAT_OptMaizeSowingDate_LONG_S1$Season<-'Season 1'
# RW_DSSAT_OptMaizeSowingDate_LONG_S1$Crop<-'Maize'
# RW_DSSAT_OptMaizeSowingDate_LONG_S1$Variety<-'Long'
# 
# 
# 
# RW_DSSAT_OptMaizeSowingDate_SHORT_S2 <- readRDS("/home/jovyan/agwise/testingSpace/DSSATtest/Output_data/RW_DSSAT_OptMaizeSowingDate_SHORT_S2.RDS")
# RW_DSSAT_OptMaizeSowingDate_SHORT_S2$Country<-'Rwanda'
# RW_DSSAT_OptMaizeSowingDate_SHORT_S2$Season<-'Season 2'
# RW_DSSAT_OptMaizeSowingDate_SHORT_S2$Crop<-'Maize'
# RW_DSSAT_OptMaizeSowingDate_SHORT_S2$Variety<-'Short'
# 
# RW_DSSAT_OptMaizeSowingDate_MEDIUM_S2 <- readRDS("/home/jovyan/agwise/testingSpace/DSSATtest/Output_data/RW_DSSAT_OptMaizeSowingDate_MEDIUM_S2.RDS")
# RW_DSSAT_OptMaizeSowingDate_MEDIUM_S2$Country<-'Rwanda'
# RW_DSSAT_OptMaizeSowingDate_MEDIUM_S2$Season<-'Season 1'
# RW_DSSAT_OptMaizeSowingDate_MEDIUM_S2$Crop<-'Maize'
# RW_DSSAT_OptMaizeSowingDate_MEDIUM_S2$Variety<-'Medium'
# 
# RW_DSSAT_OptMaizeSowingDate_LONG_S2 <- readRDS("/home/jovyan/agwise/testingSpace/DSSATtest/Output_data/RW_DSSAT_OptMaizeSowingDate_LONG_S2.RDS")
# RW_DSSAT_OptMaizeSowingDate_LONG_S2$Country<-'Rwanda'
# RW_DSSAT_OptMaizeSowingDate_LONG_S2$Season<-'Season 2'
# RW_DSSAT_OptMaizeSowingDate_LONG_S2$Crop<-'Maize'
# RW_DSSAT_OptMaizeSowingDate_LONG_S2$Variety<-'Long'
# 
# OptimumSowingdates_RwandaMaize<-rbind(RW_DSSAT_OptMaizeSowingDate_SHORT_S1, RW_DSSAT_OptMaizeSowingDate_MEDIUM_S1, RW_DSSAT_OptMaizeSowingDate_LONG_S1,
#                                       RW_DSSAT_OptMaizeSowingDate_SHORT_S2, RW_DSSAT_OptMaizeSowingDate_MEDIUM_S2, RW_DSSAT_OptMaizeSowingDate_LONG_S2)
# 
# saveRDS(OptimumSowingdates_RwandaMaize, file = "/home/jovyan/agwise/testingSpace/DSSATtest/Output_data/ConsolidatedOptimumSowingDates_Maize_Rwanda.RDS")
# 
# 
# 
# 
# 
# 
# RW_DSSAT_OptPotatoSowingDate_SHORT_S1 <- readRDS("/home/jovyan/agwise/testingSpace/DSSATtest/Output_data/RW_DSSAT_OptPotatoSowingDate_SHORT_S1.RDS")
# RW_DSSAT_OptPotatoSowingDate_SHORT_S1$Country<-'RWanda'
# RW_DSSAT_OptPotatoSowingDate_SHORT_S1$Season<-'Season 1'
# RW_DSSAT_OptPotatoSowingDate_SHORT_S1$Crop<-'Potato'
# RW_DSSAT_OptPotatoSowingDate_SHORT_S1$Variety<-'Short'
# 
# RW_DSSAT_OptPotatoSowingDate_MEDIUM_S1 <- readRDS("/home/jovyan/agwise/testingSpace/DSSATtest/Output_data/RW_DSSAT_OptPotatoSowingDate_MEDIUM_S1.RDS")
# RW_DSSAT_OptPotatoSowingDate_MEDIUM_S1$Country<-'Rwanda'
# RW_DSSAT_OptPotatoSowingDate_MEDIUM_S1$Season<-'Season 1'
# RW_DSSAT_OptPotatoSowingDate_MEDIUM_S1$Crop<-'Potato'
# RW_DSSAT_OptPotatoSowingDate_MEDIUM_S1$Variety<-'Medium'
# 
# RW_DSSAT_OptPotatoSowingDate_LONG_S1 <- readRDS("/home/jovyan/agwise/testingSpace/DSSATtest/Output_data/RW_DSSAT_OptPotatoSowingDate_LONG_S1.RDS")
# RW_DSSAT_OptPotatoSowingDate_LONG_S1$Country<-'Rwanda'
# RW_DSSAT_OptPotatoSowingDate_LONG_S1$Season<-'Season 1'
# RW_DSSAT_OptPotatoSowingDate_LONG_S1$Crop<-'Potato'
# RW_DSSAT_OptPotatoSowingDate_LONG_S1$Variety<-'Long'
# 
# RW_DSSAT_OptPotatoSowingDate_SHORT_S2 <- readRDS("/home/jovyan/agwise/testingSpace/DSSATtest/Output_data/RW_DSSAT_OptPotatoSowingDate_SHORT_S2.RDS")
# RW_DSSAT_OptPotatoSowingDate_SHORT_S2$Country<-'Rwanda'
# RW_DSSAT_OptPotatoSowingDate_SHORT_S2$Season<-'Season 2'
# RW_DSSAT_OptPotatoSowingDate_SHORT_S2$Crop<-'Potato'
# RW_DSSAT_OptPotatoSowingDate_SHORT_S2$Variety<-'Short'
# 
# RW_DSSAT_OptPotatoSowingDate_MEDIUM_S2 <- readRDS("/home/jovyan/agwise/testingSpace/DSSATtest/Output_data/RW_DSSAT_OptPotatoSowingDate_MEDIUM_S2.RDS")
# RW_DSSAT_OptPotatoSowingDate_MEDIUM_S2$Country<-'Rwanda'
# RW_DSSAT_OptPotatoSowingDate_MEDIUM_S2$Season<-'Season 1'
# RW_DSSAT_OptPotatoSowingDate_MEDIUM_S2$Crop<-'Potato'
# RW_DSSAT_OptPotatoSowingDate_MEDIUM_S2$Variety<-'Medium'
# 
# RW_DSSAT_OptPotatoSowingDate_L0NG_S2 <- readRDS("/home/jovyan/agwise/testingSpace/DSSATtest/Output_data/RW_DSSAT_OptPotatoSowingDate_LONG_S2.RDS")
# RW_DSSAT_OptPotatoSowingDate_L0NG_S2$Country<-'Rwanda'
# RW_DSSAT_OptPotatoSowingDate_L0NG_S2$Season<-'Season 2'
# RW_DSSAT_OptPotatoSowingDate_L0NG_S2$Crop<-'Potato'
# RW_DSSAT_OptPotatoSowingDate_L0NG_S2$Variety<-'Long'
# 
# OptimumSowingdates_RwandaPotato<-rbind(RW_DSSAT_OptPotatoSowingDate_SHORT_S1,RW_DSSAT_OptPotatoSowingDate_MEDIUM_S1,RW_DSSAT_OptPotatoSowingDate_LONG_S1,
#                                        RW_DSSAT_OptPotatoSowingDate_SHORT_S2,RW_DSSAT_OptPotatoSowingDate_MEDIUM_S2,RW_DSSAT_OptPotatoSowingDate_L0NG_S2)
#                                                                                                                     
# saveRDS(OptimumSowingdates_RwandaPotato, file = "/home/jovyan/agwise/testingSpace/DSSATtest/Output_data/ConsolidatedOptimumSowingDates_Potato_Rwanda.RDS")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
