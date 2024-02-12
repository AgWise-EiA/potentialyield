library(dplyr)
library(foreach)
library(doParallel)

pyield_data <- readRDS("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Kenya_KALRO/Maize/result/DSSAT/AOI/useCase_Kenya_KALRO_Maize_AOI_season_11.RDS")
names(pyield_data)
head(pyield_data)

pyield_data$year<-substr(pyield_data$PDAT,1,4)
pyield_data$date<-substr(pyield_data$PDAT,1,10)
pyield_data$doy<-strftime(pyield_data$PDAT,format="%j")
pyield_data$ydoy<-strftime(pyield_data$PDAT,format="%y%j")
pyield_data$XY<-paste0(pyield_data$Lon,"_",pyield_data$Lat)
pyield_data<-as.data.frame(pyield_data)
head(pyield_data)

Rainfall_Season_1_PointData_AOI <- readRDS("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Kenya_KALRO/Maize/result/geo_4cropModel/Rainfall_Season_1_PointData_AOI.RDS")
gps<-Rainfall_Season_1_PointData_AOI[,c(1,2,6,7)]
gps$XY<-paste0(gps$longitude,"_",gps$latitude)
head(gps)

pyd <- merge(gps, pyield_data,by='XY')


# points <- 1:nrow(gps)
# 
# points <- points -1
# 
# gps$point <-paste0('EXTE',sprintf("%04d", points))

# Merge gps and red tables together

colnames(pyd)<-c("point", "Lon", "Lat", "NAME_0", "NAME_1","NAME_2" ,"year",
                 "variety","year_doy","yield","doy")
head(pyd)

pyd_select<-pyd[,c("point","Lon", "Lat","NAME_0", "NAME_1","NAME_2" ,"year",
                   "variety","yield","doy")]
head(pyd_select)

# edist <- foreach(i = unique(1:nrow(soilm))) %do% {trial_id = euc.dist(zm[i,],soilm[i,])}
# edist <- do.call(rbind, edist)

max_yld <- foreach(k =  1:length(unique(pyd_select$point))) %do% {test <- subset(pyd_select, pyd_select$point==unique(pyd_select$point)[k])
test <- subset(test, test$variety !='EX899996')
test <- test %>% arrange(desc(yield))
test <- test[1:3,]
test$yld_option <- c(1:nrow(test))
}
max_yld <- do.call(rbind, max_yld)


ymax_pk <- NULL
for (k in 1:length(unique(pyd_select$point))){
  pk <- subset(pyd_select, pyd_select$point==unique(pyd_select$point)[k])
  pk <-  subset(pk, pk$variety!='EX899996')
  ymax <- order(pk$yield, decreasing = TRUE)[1:4]
  ymax <- pk[ymax,]
  ymax$yield_option <- c(1:4)
  ymax_pk <- rbind(ymax_pk,ymax)
}

ymax_pk$doy[ymax_pk$doy==338]<- '1-7 Dec'
ymax_pk$doy[ymax_pk$doy==339]<- '2-8 Dec'
ymax_pk$doy[ymax_pk$doy==345]<- '7-14 Dec'

ymax_pk$variety[ymax_pk$variety=="EX880002"]<- 'Medium'
ymax_pk$variety[ymax_pk$variety=="EX880004"]<- 'Long'
ymax_pk$variety[ymax_pk$variety=="EX880001"]<- 'Short'

ymax_pk1<-ymax_pk[,-c(1,7,9)]
colnames(ymax_pk1)<-c("Lon"," Lat"," NAME_0", "NAME_1","  NAME_2","variety_type", "Planting_window", "Best_planting_option")

head(ymax_pk1,5)
write.csv(ymax_pk1,'~/transform-modelling/Opt_sowing_dates_Solidaridad_Chinyanja_Trianagle.csv')

