library('tidyverse')
library('geodata')
library(sf)

###### Read summary rainfall data ####
path <- "~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Rwanda_RAB/Potato/raw/Rainfall/"
CHIRPS_summary <- readRDS(paste0(path,"Rainfall_summaries_trial_CHIRPS.RDS"))
AgEra_summary <- readRDS(paste0(path,"Rainfall_summaries_trial_AgEra.RDS"))
CHIRPS_summary$source <- "CHIRPS"
AgEra_summary$source <- "AgEra"
summary <- bind_rows(CHIRPS_summary,AgEra_summary)
ggplot(summary,aes(x=NAME_2,y=nrRainyDays, fill=source)) +
  geom_boxplot()+ theme_bw()

ggplot(summary,aes(x=NAME_2,y=totalRF, fill=source)) +
  geom_boxplot()+ theme_bw()+labs(y="Total rainfall (mm)",x="District")+
  ylim(0,1500)+
  annotate("text",
           x = 1:length(table(AgEra_summary$NAME_2)),
           y = aggregate(totalRF ~ NAME_2, AgEra_summary, max)[ , 2],
           label = table(AgEra_summary$NAME_2),
           col = "red",
           vjust = - 1)

ggplot(summary,aes(x=NAME_2,y=Rain_month1, fill=source)) +
  geom_boxplot()+ theme_bw()+labs(y="Rain month 1 (mm)",x="District")

ggplot(summary,aes(x=NAME_2,y=Rain_month2, fill=source)) +
  geom_boxplot()+ theme_bw()+labs(y="Rain month 2 (mm)",x="District")


ggplot(summary,aes(x=NAME_2,y=Rain_month3, fill=source)) +
  geom_boxplot()+ theme_bw()+labs(y="Rain month 3 (mm)",x="District")

ggplot(summary,aes(x=NAME_2,y=Rain_month4, fill=source)) +
  geom_boxplot()+ theme_bw()+labs(y="Rain month 4 (mm)",x="District")

ggplot(summary,aes(x=NAME_2,y=Rain_month5, fill=source)) +
  geom_boxplot()+ theme_bw()+labs(y="Rain month 5 (mm)",x="District")

##Plot locations 
rwa2 <- gadm(country="RWA", level=2, path=tempdir())

rwa2 <- rwa2 %>% 
  st_as_sf() %>%
  mutate(
    lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
    lat = map_dbl(geometry, ~st_centroid(.x)[[2]])
  )

summary_sf <- st_as_sf(summary, coords=c("longitude","latitude"),crs="WGS84")

ggplot() + 
  geom_sf(data=rwa2) +
  geom_sf(data=summary_sf)+
  geom_text(data=rwa2,aes(label = NAME_2, x = lon, y = lat), size = 2)+
  theme_bw()

###### Read daily rainfall data ####
CHIRPS_point <- readRDS(paste0(path,"Rainfall_pointData_trial_CHIRPS.RDS"))
AgEra_point <- readRDS(paste0(path,"Rainfall_pointData_trial_AgEra.RDS"))
CHIRPS_point$source <- "CHIRPS"
AgEra_point$source <- "AgEra"

point_rain <-bind_rows(CHIRPS_point,AgEra_point)
point_rain <- point_rain %>%
  pivot_longer(cols = starts_with("Rain"), names_to = c('variable', 'day'), names_sep="_")

point_rain$value <- as.numeric(point_rain$value)
point_rain$day <- as.numeric(point_rain$day)

ggplot(point_rain, aes(value,colour=source))+
  geom_density()+facet_wrap(~NAME_2)+xlim(0,30)+theme_bw()

point_rain$date <- as.Date(point_rain$day,    # Convert Julian day to date
                   origin = as.Date("2020-01-01"))

point_rain$week <- week(point_rain$date)
point_rain$month <- month(point_rain$date)

point_rain_week <- point_rain %>% group_by(source,NAME_2,longitude,latitude,week) %>%
  summarise(sum_rain=sum(value,na.rm=T))

ggplot(point_rain_week, aes(x=as.factor(week),y=sum_rain,fill=source))+
  geom_boxplot()+theme_bw()+labs(x="Week of the year",y= "Weekly rainfall (mm)") 

point_rain %>% group_by(source,NAME_2,longitude,latitude,month) %>%
  summarise(sum_rain=sum(value,na.rm=T))%>%
  ggplot(aes(x=as.factor(month),y=sum_rain,fill=source))+
  geom_boxplot()+theme_bw()+labs(x="Month",y= "Monthly rainfall (mm)") +facet_wrap(~NAME_2)

#### Observed data from Kigali airport ####
kigali <- read.csv(paste0(path,"kigali_airport.csv"))
kigali_sf <- st_as_sf(kigali, coords=c("LONGITUDE","LATITUDE"),crs="WGS84")
ggplot() + 
  geom_sf(data=rwa2) +
  geom_sf(data=summary_sf)+
  geom_sf(data=kigali_sf,colour="blue",size=4)+
  geom_text(data=rwa2,aes(label = NAME_2, x = lon, y = lat), size = 2)+
  theme_bw()

kigali$DATE <- as.Date(kigali$DATE)
kigali$week <- week(kigali$DATE)
kigali$month <- month(kigali$DATE)
kigali$year <- year(kigali$DATE)

kigali %>% group_by(year,month) %>%
  summarise(sum_rain=sum(PRCP,na.rm=T))%>%
  ggplot(aes(x=as.factor(month),y=sum_rain,fill=as.factor(year)))+
  geom_bar(stat="identity",colour="black", position=position_dodge())+
  theme_bw()+labs(x="Month",y= "Monthly rainfall (mm)",fill="year") 

kigali %>% group_by(year,week) %>%
  summarise(sum_rain=sum(PRCP,na.rm=T))%>%
  ggplot(aes(x=as.factor(week),y=sum_rain))+ facet_wrap(~year)+
  geom_bar(stat="identity",colour="black", position=position_dodge())+
  theme_bw()+labs(x="Week",y= "Weekly rainfall (mm)") 
