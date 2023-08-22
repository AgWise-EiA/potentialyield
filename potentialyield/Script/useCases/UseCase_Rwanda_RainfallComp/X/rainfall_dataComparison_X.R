library('tidyverse')
library('geodata')
library(sf)

###### Read summary rainfall data ####
path1 <- "~/agwise-datacuration/dataops/datacuration/Data/useCase_Rwanda_RainfallComp/X/result/"
result <- readRDS(paste0(path1,"/weatherDataStation.RDS"))
result$Weather_Date <-as.Date(result$Weather_Date,format="%m/%d/%Y")
result$month <- month(result$Weather_Date)

test<-result %>%
  filter (between(Weather_Date,plantingDate, harvestDate))  %>%
  group_by(oldID,ID,plantingDate) %>%
  summarise(nrRainyDays = sum(Rainfall.mm > 0,is.na=F))

test2<-result %>%
  filter (between(Weather_Date,plantingDate, harvestDate))  %>%
  group_by(oldID,ID,plantingDate,month) %>%
  summarise(Rain = sum(Rainfall.mm)) %>%
  pivot_wider(names_from=month,values_from=Rain,names_prefix="Rain_month")

test2[4,c(4:15)] <- test2[4,c(13:15,4:12)]

test3<-result %>%
  filter (between(Weather_Date,plantingDate, harvestDate))  %>%
  group_by(oldID,ID,plantingDate) %>%
  summarise(totalRF = sum(Rainfall.mm))


df_list <- list(test, test2, test3)

#merge all data frames in list
summary <-df_list %>% reduce(full_join)
summary <-summary[summary$ID!=4,] #Currently airport data is not working
summary$ID <- summary$oldID
summary$source <-"Observed"

path <- "~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Rwanda_RainfallComp/X/raw/Rainfall/"
CHIRPS_summary <- readRDS(paste0(path,"Rainfall_summaries_trial_CHIRPS.RDS"))
AgEra_summary <- readRDS(paste0(path,"Rainfall_summaries_trial_AgEra.RDS"))
CHIRPS_summary$source <- "CHIRPS"
AgEra_summary$source <- "AgEra"

minifile <-AgEra_summary[c("ID","NAME_1","NAME_2")]
summary<- merge(minifile,summary)
summary_all <- bind_rows(CHIRPS_summary,AgEra_summary,summary)

summary_all <- summary_all[summary_all$ID !="4_2017",]
ggplot(summary_all,aes(x=ID,y=nrRainyDays, fill=source)) +
  geom_bar(stat="identity",colour="black", position=position_dodge())+ theme_bw()+
  labs(y="Nr. of rainy days",x="Locations")

ggplot(summary_all,aes(x=ID,y=totalRF, fill=source)) +
  geom_bar(stat="identity",colour="black", position=position_dodge())+ theme_bw()+
  labs(y="Total rainfall (mm)",x="Locations")

alldata_longer <- summary_all %>%
  pivot_longer(cols = starts_with("Rain"), names_to = c('variable', 'month'), names_sep="_month")

ggplot(subset(alldata_longer,ID=="1_2016"),aes(x=as.numeric(month),y=value, fill=source)) +
  geom_bar(stat="identity",colour="black", position=position_dodge())+ theme_bw()+
  labs(y="Rainfall (mm)",x="Month")+ scale_x_continuous(breaks=c(1:13))

ggplot(subset(alldata_longer,ID=="1_2017"),aes(x=as.numeric(month),y=value, fill=source)) +
  geom_bar(stat="identity",colour="black", position=position_dodge())+ theme_bw()+
  labs(y="Rainfall (mm)",x="Month")+ scale_x_continuous(breaks=c(1:13))

ggplot(subset(alldata_longer,ID=="3"),aes(x=as.numeric(month),y=value, fill=source)) +
  geom_bar(stat="identity",colour="black", position=position_dodge())+ theme_bw()+
  labs(y="Rainfall (mm)",x="Month")+ scale_x_continuous(breaks=c(1:13))

##Plot locations 
rwa2 <- gadm(country="RWA", level=2, path=tempdir())

rwa2 <- rwa2 %>% 
  st_as_sf() %>%
  mutate(
    lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
    lat = map_dbl(geometry, ~st_centroid(.x)[[2]])
  )

summary_map <- summary_all[!is.na(summary_all$longitude),]
summary_sf <- st_as_sf(summary_map, coords=c("longitude","latitude"),crs="WGS84")

ggplot() + 
  geom_sf(data=rwa2) +
  geom_sf(data=summary_sf)+
  geom_text(data=rwa2,aes(label = NAME_2, x = lon, y = lat), size = 2)+
  theme_bw()

