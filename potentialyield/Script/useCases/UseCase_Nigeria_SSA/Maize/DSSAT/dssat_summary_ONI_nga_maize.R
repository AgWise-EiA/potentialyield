# Aggregated DSSAT output and ONI classification of aggregated DSSAT output

# Introduction: 
# This script allows : 
# (1) to aggregate/merge DSSAT output
# (2) the classification of aggregated ouput from DSSAT simulation in terms of ONI type : Nina, Nino and Neutral year
# It provides also graphical ouputs of aggregated DSSAT output according to the ONI
# For more info regarding ONI : https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php
# Authors : P.Moreno, E.Bendito Garcia, L.Leroux
# Credentials : EiA, 2024

#### Getting started #######

country="Nigeria"
useCaseName="SSA"
Crop = "Maize"
Extent = "AOI"
Season = 1
Plot = TRUE


# 1. Sourcing required packages -------------------------------------------
packages_required <- c("DSSAT","tidyverse","dplyr", "purrr", "ggridges","patchwork", "Rmisc", "terra", "cowplot")

installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  if(packages_required[!installed_packages] =="DSSAT"){
    remotes::install_github("palderman/DSSAT", ref = "develop",force=T)
  } else {
    install.packages(packages_required[!installed_packages])
  }
}

lapply(packages_required, library, character.only = TRUE)

# load required packages
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))

# 2. Defining required functions -------------------------------------------
get_median_variable <- function(initial_date, final_date, variable, data) {
  filtered_data <- data %>%
    filter(date >= as.Date(initial_date) & date <= as.Date(final_date))
  
  med_variable <- median(filtered_data[[variable]], na.rm = TRUE)
  
  return(data.frame(initial_date = initial_date, final_date = final_date, med_variable = med_variable))
}

# Read data
pathOut <- paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName,"/",Crop,"/result/DSSAT/",Extent,"/", sep="")



oni_map <- function(data, x,y, fill, HWAH, shp, limits){
  
  # Mean plot
  if (HWAH == TRUE){
    ggplot(data = data) +
      geom_raster(aes(x = {{x}}, y = {{y}}, fill = {{fill}})) +
      facet_grid(cols = vars(Wheather), switch=c('y'), labeller=as_labeller(c(c(`A`="Niño",`B`="Neutral", `C`="Niña"),c(`A`="Niño",`B`="Neutral", `C`="Niña"))))+
      scale_fill_stepsn(n.breaks = 9, colours = viridis::viridis(9),name=" Yield"~(kg~ha^{-1}), limits = limits)+ theme_bw()+
      theme(legend.position = "right")+ 
      geom_sf(data=shp, fill=NA, color="white", linewidth=0.5)+
      coord_sf(expand = FALSE, xlim=c(min(data$Long), max(data$Long)), ylim=c(min(data$Lat), max(data$Lat)))+
      xlab("Longitude")+ ylab("Latitude") + ggtitle(label="Mean")
  } else {
    ggplot(data = data) +
      geom_raster(aes(x = {{x}}, y = {{y}}, fill = {{fill}})) +
      facet_grid(rows = vars(Wheather), switch=c('y'), labeller=as_labeller(c(`A`="long",`B`="medium", `C`="short"), c(`A`="Niño",`B`="Neutral", `C`="Niña")))+
      scale_fill_stepsn(n.breaks = 9, colours = viridis::magma(9),name=" Yield"~(kg~ha^{-1}), limits = limits)+ theme_bw()+
      theme(legend.position = "right")+ 
      geom_sf(data=shp, fill=NA, color="white", linewidth=0.5)+
      coord_sf(expand = FALSE, xlim=c(min(data$Long), max(data$Long)), ylim=c(min(data$Lat), max(data$Lat)))+
      xlab("Longitude")+ ylab("Latitude") + ggtitle(label="Standard Deviation")
  }
}


oni_map_c <- function(data, x,y, fill, HWAH, shp, limits){
  
  # Mean plot
  if (HWAH == TRUE){
    ggplot(data = data) +
      geom_raster(aes(x = {{x}}, y = {{y}}, fill = {{fill}})) +
      #facet_grid(cols = vars(Wheather), switch=c('x'), labeller=as_labeller(c(`A`="Niño",`B`="Neutral", `C`="Niña")))+
      facet_grid(Variety~Wheather, labeller=labeller(c(`A`="long",`B`="medium", `C`="short"), c(`i`="Niño",`ii`="Neutral", `iii`="Niña")))+
      scale_fill_stepsn(n.breaks = 9, colours = viridis::viridis(9),name=" Yield"~(kg~ha^{-1}), limits = limits)+ theme_bw()+
      theme(legend.position = "right")+ 
      geom_sf(data=shp, fill=NA, color="white", linewidth=0.5)+
      coord_sf(expand = FALSE, xlim=c(min(data$Long), max(data$Long)), ylim=c(min(data$Lat), max(data$Lat)))+
      xlab("Longitude")+ ylab("Latitude") + ggtitle(label="Mean") + theme(plot.title = element_text(hjust = 0.5))
  } else {
    ggplot(data = data) +
      geom_raster(aes(x = {{x}}, y = {{y}}, fill = {{fill}})) +
      #facet_grid(cols = vars(Wheather), switch=c('x'), labeller=as_labeller(c(`A`="Niño",`B`="Neutral", `C`="Niña")))+
      facet_grid(Variety~Wheather, labeller=labeller(c(`A`="long",`B`="medium", `C`="short"), c(`i`="Niño",`ii`="Neutral", `iii`="Niña")))+
      scale_fill_stepsn(n.breaks = 9, colours = viridis::magma(9),name=" Yield"~(kg~ha^{-1}), limits = limits)+ theme_bw()+
      theme(legend.position = "right")+ 
      geom_sf(data=shp, fill=NA, color="white", linewidth=0.5)+
      coord_sf(expand = FALSE, xlim=c(min(data$Long), max(data$Long)), ylim=c(min(data$Lat), max(data$Lat)))+
      xlab("Longitude")+ ylab("Latitude") + ggtitle(label="Standard Deviation") + theme(plot.title = element_text(hjust = 0.5))
  }
}

dssat_oni <- readRDS(paste0(pathOut, 'useCase_', country,'_', useCaseName, '_', Crop, '_', Extent,'_season_',Season, '_ONI.RDS'))

# Rename varieties
  v_levs <- unique(dssat_oni$Variety)
  dssat_oni$Variety <- gsub(v_levs[1], "short", dssat_oni$Variety)
  dssat_oni$Variety <- gsub(v_levs[2], "medium", dssat_oni$Variety)
  dssat_oni$Variety <- gsub(v_levs[3], "long", dssat_oni$Variety)
 #  
  # # med ONI > 0.5, Nino, med ONI < -0.5 Nina, -0.5 > ONI > 0.5 Neutral 
  dssat_oni$ENSO <- ifelse(dssat_oni$med_variable > 0.5,"Niña",
                           ifelse(dssat_oni$med_variable< -0.5,"Niño", "Neutral"))

  
  z <- which(dssat_oni$HWAH==0 & is.na(dssat_oni$ENSO) == TRUE)
 
    val <- c("Neutral" = "gold",
             "Niño" = "tomato1",
             "Niña" = "royalblue2")
  
    x_lab_o <- unique(dssat_oni$TNAM)

  # Meand and SEM plot
  pd <- position_dodge(0.2)
  dssat_oni %>% 
    filter(ENSO!='NA') %>%
    summarySE(measurevar="HWAH", groupvars=c("Variety","TNAM", "ENSO"))%>%
  ggplot(aes(x = factor(TNAM, level= x_lab_o), 
             y = HWAH, 
             group=ENSO, 
             color=ENSO)) +
    geom_point(position = pd, size = 3) +
    geom_line(position = pd,linewidth = 1) +
    geom_errorbar(aes(ymin = HWAH - se, ymax = HWAH + se), width = .1, position= pd)+
    #theme_bw()+ ylim(0, 15000) +
    scale_color_manual(values=val)+ theme(axis.text.x =element_text(angle=45, hjust=1))+
    ylab(expression(bold('Yield'~(kg~ha^{-1})))) + xlab(expression(bold("Planting time")))
  ggsave(paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_",Extent,"_season_",Season,"_ONI_DotPlot_Global.pdf"))

  # Heat map
  p1 <- dssat_oni %>%
    filter(ENSO!='NA') %>%
    summarySE(measurevar="HWAH", groupvars=c("Variety","TNAM", "ENSO"))%>%
  ggplot(aes(x=factor(TNAM, level= x_lab_o), y=ENSO, fill=HWAH))+
     geom_tile(color="white", linewidth=0.1)+
     theme_bw()+
     theme(axis.text.x =element_text(angle=45, hjust=1))+
     scale_fill_viridis_c(name="Yield"~(kg~ha^{-1}))+
     coord_equal()+
    ylab(expression(bold("ENSO")))+xlab(expression(bold("Planting time")))+
    ggtitle("Mean yield")
  
 p2 <-  dssat_oni %>%
   filter(ENSO!='NA') %>%
   summarySE(measurevar="HWAH", groupvars=c("Variety","TNAM", "ENSO"))%>%
    ggplot(aes(x=factor(TNAM, level= x_lab_o), y=ENSO, fill=sd))+
    geom_tile(color="white", linewidth=0.1)+
    theme_bw()+
    theme(axis.text.x =element_text(angle=45, hjust=1))+
    scale_fill_viridis_c(name="Yield"~(kg~ha^{-1}))+
    coord_equal()+
    ylab(expression(bold("ENSO")))+xlab(expression(bold("Planting time")))+
    ggtitle("Standard Deviation")
 p1+p2+plot_layout(ncol = 1) 
 ggsave(paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_",Extent,"_season_",Season,"_ONI_HeatMap_Global.pdf"))
 
 ### 4.5.2. Plot by variety ####
 # Meand and SEM plot
 pd <- position_dodge(0.2)
 dssat_oni %>%
   filter(ENSO!='NA') %>%
   summarySE(measurevar="HWAH", groupvars=c("Variety","TNAM", "ENSO"))%>%
   ggplot(aes(x = factor(TNAM, level= x_lab_o), 
              y = HWAH, 
              group=ENSO, 
              color=ENSO)) +
   facet_grid(rows = vars(Variety))+
   geom_point(position = pd, size = 3) +
   geom_line(position = pd,linewidth = 1) +
   geom_errorbar(aes(ymin = HWAH - se, ymax = HWAH + se), width = .1, position= pd)+
   theme_bw()+
   scale_color_manual(values=val)+ theme(axis.text.x =element_text(angle=45, hjust=1))+
   ylab(expression(bold('Yield'~(kg~ha^{-1})))) + xlab(expression(bold("Planting time")))
 ggsave(paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_",Extent,"_season_",Season,"_ONI_DotPlot_Variety.pdf"))
 
 # Heat map
 p1 <- dssat_oni %>%
   filter(ENSO!='NA') %>%
   summarySE(measurevar="HWAH", groupvars=c("Variety","TNAM", "ENSO"))%>%
   ggplot(aes(x=factor(TNAM, level= x_lab_o), y=ENSO, fill=HWAH))+
   facet_grid(rows = vars(Variety))+
   geom_tile(color="white", linewidth=0.1)+
   theme_bw()+
   theme(axis.text.x =element_text(angle=45, hjust=1))+
   scale_fill_viridis_c(name="Yield"~(kg~ha^{-1}))+
   coord_equal()+
   ylab(expression(bold("ENSO")))+xlab(expression(bold("Planting time")))+
   ggtitle("Mean yield")
 
 p2 <-  dssat_oni %>%
   filter(ENSO!='NA') %>%
   summarySE(measurevar="HWAH", groupvars=c("Variety","TNAM", "ENSO"))%>%
   ggplot(aes(x=factor(TNAM, level= x_lab_o), y=ENSO, fill=sd))+
   facet_grid(rows = vars(Variety))+
   geom_tile(color="white", linewidth=0.1)+
   theme_bw()+
   theme(axis.text.x =element_text(angle=45, hjust=1))+
   scale_fill_viridis_c(name="Yield"~(kg~ha^{-1}))+
   coord_equal()+
   ylab(expression(bold("ENSO")))+xlab(expression(bold("Planting time")))+
   ggtitle("Standard Deviation")
 p1+p2+plot_layout(ncol = 2) 
 ggsave(paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_",Extent,"_season_",Season,"_ONI_HeatMap_Variety.pdf"))
 
 ## 4.6. Maps ####
 
 # Read the relevant shape file from gdam to be used to crop the global data
 countryShp <- geodata::gadm(country, level = 1, path=paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName,"/",Crop,"/result/DSSAT/", sep=""))
 country_sf <- sf::st_as_sf(countryShp)
 
 prv <- which(country_sf$NAME_1 %in% unique(dssat_oni$Loc))
 
 #country_sf <- country_sf[prv,]
 
 ### 4.6.1. Global Map ####
  dssat_oni.g <- dssat_oni %>%
   summarySE(measurevar="HWAH", groupvars=c("Lat","Long", "ENSO", "Variety"))
 
 # Scale limits
 min.mean <- min(dssat_oni.g$HWAH)
 max.mean <- max(dssat_oni.g$HWAH)
 
 min.sd <- min(dssat_oni.g$sd)
 max.sd <- max(dssat_oni.g$sd)
 
 ## Neutral
 dssat.neutral.g <- subset(dssat_oni.g, dssat_oni.g$ENSO %in% 'Neutral', select = -c(ENSO, N))
 dssat.neutral.g <- na.omit(dssat.neutral.g[,c(2,1,3,4,5,6,7)])
 
 ## Nino
 dssat.nino.g <- subset(dssat_oni.g, dssat_oni.g$ENSO %in% 'Niño', select = -c(ENSO, N))
 dssat.nino.g <- na.omit(dssat.nino.g[,c(2,1,3,4,5,6,7)])
 
 ## Nina
 dssat.nina.g <- subset(dssat_oni.g, dssat_oni.g$ENSO %in% 'Niña', select = -c(ENSO, N))
 dssat.nina.g <- na.omit(dssat.nina.g[,c(2,1,3,4,5,6,7)])
 
 ## Assembling
 dssat.g.mean <- dssat.neutral.g
 dssat.g.mean$Wheather <- "Neutral"
 dssat.nino.g$Wheather <- 'Nino'
 dssat.nina.g$Wheather <- 'Nina'
 dssat.g.mean <- rbind(dssat.g.mean, dssat.nino.g)
 dssat.g.mean <- rbind(dssat.g.mean, dssat.nina.g)
 
 mean.g <-oni_map(data=dssat.g.mean, x=longlat.scales(), y=Lat,shp=country_sf, fill=HWAH, HWAH= TRUE,limits=c(min.mean, max.mean))
 sd.g <-oni_map(data=dssat.g.mean, x=Long, y=Lat,shp=country_sf, fill=sd, HWAH= FALSE, limits=c(min.sd, max.sd))
 
 mean.gc <-oni_map_c(data=dssat.g.mean, x=Long, y=Lat,shp=country_sf, fill=HWAH, HWAH= TRUE,limits=c(min.mean, max.mean))
 mean.gc
 ggsave(paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_",Extent,"_season_",Season,"_ONI_Maps_Global_mean.pdf"), dpi=300, width = 8, height=6.89, units=c("in"))
 
 sd.gc <-oni_map_c(data=dssat.g.mean, x=Long, y=Lat,shp=country_sf, fill=sd, HWAH= FALSE, limits=c(min.sd, max.sd))
 sd.gc
 ggsave(paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_",Extent,"_season_",Season,"_ONI_Maps_Global_sd.pdf"), dpi=300, width = 8, height=6.89, units=c("in"))
 
 