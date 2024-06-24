#library(dplyr)
library(foreach)
library(tidyverse)
library(purrr)
library(data.table)
library(DBI)
library(RSQLite)


# country="Malawi"
# useCaseName="Solidaridad"
# Crop = "Maize"
# Extent = "AOI"
# Season = 1
# Plot = TRUE

# pyield_data <- readRDS("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Malawi_Solidaridad/Soybean/result/DSSAT/AOI/useCase_Malawi_Solidaridad_Soybean_AOI_season_1_ONI.RDS")
pyield_data <- as.data.table(readRDS(("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_ZambeziBasin_Solidaridad/Maize/dssat_oni.RDS")))
names(pyield_data)
head(pyield_data,3)

# pyield_data$year<-substr(pyield_data$PDAT,1,4)
# pyield_data$date<-substr(pyield_data$PDAT,1,10)
# pyield_data$doy<-strftime(pyield_data$PDAT,format="%j")
# pyield_data$ydoy<-strftime(pyield_data$PDAT,format="%y%j")
# pyield_data$XY<-paste0(pyield_data$Long,"_",pyield_data$Lat)
# pyield_data<-as.data.frame(pyield_data)
# head(pyield_data)

#Test with dplyr but it is taking too long
# pyield_data <- pyield_data %>%
#   mutate(
#     year = map(PDAT, ~ year(.)), 
#     #date = str_sub(PDAT, 1, 10),
#     doy = map(PDAT, ~ yday(ymd(.))),
#     ydoy = map(PDAT, ~ str_c(str_sub(., 1, 4), sprintf("%03d", yday(ymd(.))))),
#     XY = str_c(Long, "_", Lat)
#   )

# Create a SQLite database connection
con <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(con, "large_table", pyield_data)

# Perform operations using dplyr
result <- tbl(con, "large_table") %>%
    mutate(
      year = map(PDAT, ~ year(.)),
      #date = str_sub(PDAT, 1, 10),
      doy = map(PDAT, ~ yday(ymd(.))),
      ydoy = map(PDAT, ~ str_c(str_sub(., 1, 4), sprintf("%03d", yday(ymd(.))))),
      XY = str_c(Long, "_", Lat)
    ) %>%
  collect()

dbDisconnect(con)



#       country   NAME_1               NAME_2    lon     lat
# 121   Malawi   Nsanje           TA Ndamera 35.125 -17.125
gps_mal <- readRDS("~/agwise-datacuration/dataops/datacuration/Data/useCase_Malawi_Solidaridad/Maize/result/AOI_GPS.RDS")
gps_moz <- readRDS("~/agwise-datacuration/dataops/datacuration/Data/useCase_Mozambique_Solidaridad/Maize/result/AOI_GPS.RDS")
gps_zam <- readRDS("~/agwise-datacuration/dataops/datacuration/Data/useCase_Zambia_Solidaridad/Maize/result/AOI_GPS.RDS")
gps_zim <- readRDS("~/agwise-datacuration/dataops/datacuration/Data/useCase_Zimbabwe_Solidaridad/Maize/result/AOI_GPS.RDS")

gps<-rbind(gps_mal,gps_moz,gps_zam,gps_zim)


gps$XY<-paste0(gps$lon,"_",gps$lat)
head(gps)

pyd <- merge(gps, pyield_data,by='XY')
names(pyd)
head(pyd)

pyd$ONI<-pyd$med_variable
pyd$NAME_0<-pyd$country
pyd$yield <-pyd$HWAH
pyd$province <-pyd$Loc

# pyd_select<-pyd[,c("XY","NAME_0", "NAME_1","NAME_2","lon","lat","TRNO","TNAM","PDAT",
#                    "yield","Variety","prov","ONI","ENSO","year","date","doy",
#                    "ydoy")]

pyd_select<-pyd[,c("XY","country","province", "NAME_1","NAME_2","lon","lat","TRNO","TNAM","PDAT",
                   "yield","Variety","ONI","ENSO","year","date","doy", "ydoy")]

# summary_pyd<-pyd_select %>%  group_by(XY,lon,lat,Variety,ENSO,TRNO,TNAM,doy) %>%
#   summarise(median = median(yield))

summary_pyd<-pyd_select %>%  group_by(XY,country,province,lon,lat,Variety,ENSO,TRNO,TNAM,doy) %>%
  summarise(median = median(yield))

#get only top 3 optimum dates per ENSO per variety per location
# max_median_summary_poptions <- summary_pyd %>%  
#   group_by(XY,lon,lat,Variety,ENSO) %>% 
#   slice_max(median,n=5) %>%
#   select(TRNO,TNAM,median, doy) %>%
#   arrange(XY,ENSO,Variety, desc(median))%>%
#   filter(!is.na(ENSO))

max_median_summary_POptions <- summary_pyd %>%  
  # group_by(XY,lon,lat,Variety,ENSO) %>% 
  group_by(XY,country,province,lon,lat,Variety,ENSO) %>% 
  slice_max(median,n=5) %>%
  select(TRNO,TNAM,median, doy) %>%
  arrange(XY,country,province,ENSO,Variety, desc(median))%>%
  # select(TRNO,TNAM,median, doy) %>%
  # arrange(XY,ENSO,Variety, desc(median))%>%
  filter(!is.na(ENSO))
saveRDS(max_median_summary_POptions,'~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Chinyangya_Solidaridad/Maize/useCase_ZambeziBasin_Solidaridad_Maize_OptSowingDates_AOI_season_1_ONI.RDS')

#get only one optimum date list
max_median_summary <- summary_pyd %>%
  # group_by(XY,lon,lat,Variety,ENSO) %>%
  group_by(XY,country,province,lon,lat,Variety,ENSO) %>%
  slice_max(median, with_ties = FALSE) %>%
  select(TRNO,TNAM,median, doy) %>%
  arrange(XY,country,province,ENSO,Variety, desc(median))%>%
  # select(TRNO,TNAM,median, doy) %>%
  # arrange(XY,ENSO,Variety, desc(median))%>%
  filter(!is.na(ENSO))

###############################
# max_median_summary <- max_median_summary %>%  
#   filter(country=='Malawi') 

countries<-c("Malawi","Zambia", "Zimbabwe","Mozambique")
country_sfs<-NULL
for (k in 1:length(countries)){
countryShp <- geodata::gadm(countries[k], level = 1, path=paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", countries[k], "_",useCaseName,"/",Crop,"/result/DSSAT/", sep=""))
country_sf <- sf::st_as_sf(countryShp)
country_sfs<-bind_rows(country_sfs,country_sf)
}
country_sf<-country_sfs
country_sfs<-NULL
# country_sf0<-country_sf %>%
#   select("GID_0", "geometry")
country_sf0<-country_sf %>%
  group_by(GID_0) %>%
  summarise(geometry=sf::st_union(geometry))
  
max_median_summary <- max_median_summary %>%
  mutate(Variety = case_when(Variety=="999911" ~ "Short", Variety == "999912" ~ "Medium", Variety == "999913" ~ "Long"))

year <- 2023
max_median_summary$doyy <- as.Date(as.numeric(as.character(max_median_summary$doy))-1, origin = paste0(year, "-01-01"))
# max_median_summary$doy <- format(max_median_summary$doyy, "%m-%b")
max_median_summary$Opt_date <- format(max_median_summary$doyy, "%d-%b")

max_median_summary$TRNO <-factor(max_median_summary$TRNO)
# max_median_summary <- max_median_summary[order(max_median_summary$TRNO),]

max_median_summary$Opt_date <- factor(max_median_summary$Opt_date, 
                                      levels = unique(max_median_summary$Opt_date[order(max_median_summary$TRNO)]))

d<-max_median_summary %>%
  ggplot() +
  geom_raster(aes(x = lon, y = lat, fill = Opt_date)) +
  labs(fill ="Opt. Date")+
  scale_fill_viridis_d()+
  facet_grid(Variety~ENSO)+
  theme_bw()+
  theme(legend.position = "right", aspect.ratio = 1, 
        axis.text.x = element_text(angle = 90, hjust = 1,size = 14, face ="bold"),
        axis.text.y = element_text(size = 14, face ="bold"),
        axis.title = element_text(size = 16, face = "bold"),
        strip.text = element_text(size = 16, face = "bold"),
        strip.background = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16, face = "bold"))+ 
  geom_sf(data=country_sf, fill=NA, color="grey", linewidth=0.5)+
  geom_sf(data=country_sf0, fill=NA, color="black", linewidth=1)+
  coord_sf(xlim=c(min(max_median_summary$lon), max(max_median_summary$lon)), 
           ylim=c(min(max_median_summary$lat), max(max_median_summary$lat)))+
  xlab("Longitude")+ ylab("Latitude")
d
ggsave(plot=d,"facet_plot.png", width = 12, height = 12)

# summary_table <- max_median_summary %>%  group_by(ENSO, Variety,Opt_date) %>%  summarise(count = n())
# ###############################################################################

# 
# # 
# # oni_map(max_median_summary,x=Lon,y=Lat,fill=doy,shp=country_sf,factor1=variety,factor2=ENSO)
# # 
# # max_median_summary$doy<-max_median_summary
# 
