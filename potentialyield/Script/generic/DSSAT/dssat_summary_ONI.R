# Aggregated DSSAT output and ONI classification of aggregated DSSAT output

# Introduction: 
# This script allows : 
# (1) to aggregate/merge DSSAT output
# (2) To classify aggregated outputs from DSSAT simulation in terms of ONI index: Nina, Nino and Neutral year
# It provides also graphics of aggregated DSSAT output according to the ONI
# For more info regarding ONI : https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php
# Authors : P.Moreno, E.Bendito Garcia, L.Leroux
# Credentials : EiA, 2024

#### Getting started #######

# 1. Sourcing required packages -------------------------------------------
packages_required <- c("DSSAT","tidyverse", "ggridges","patchwork", "Rmisc", "terra", "cowplot", "foreach","doParallel")

installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
   install.packages(packages_required[!installed_packages])
  }

# load required packages
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))

# get number of cores and set for parallel computing
num_cores <- parallel::detectCores() - 1
registerDoParallel(num_cores)

# 2. Defining required functions -------------------------------------------

get_median_variable <- function(initial_date, final_date, variable, data) {
  filtered_data <- data %>%
    filter(date >= as.Date(initial_date) & date <= as.Date(final_date))
  
  med_variable <- median(filtered_data[[variable]], na.rm = TRUE)
  
  return(data.frame(initial_date = initial_date, final_date = final_date, med_variable = med_variable))
}

oni_map <- function(data, x,y, fill, HWAH, shp, limits){
  
  # Mean plot
  if (HWAH == TRUE){
    ggplot(data = data) +
      geom_raster(aes(x = {{x}}, y = {{y}}, fill = {{fill}})) +
      facet_grid(rows = vars(Wheather), switch=c('y'), labeller=as_labeller(c(`A`="Niño",`B`="Neutral", `C`="Niña")))+
      scale_fill_stepsn(n.breaks = 9, colours = viridis::viridis(9),name=" Yield"~(kg~ha^{-1}), limits = limits)+ theme_bw()+
      theme(legend.position = "right")+ 
      geom_sf(data=shp, fill=NA, color="white", linewidth=0.5)+
      coord_sf(expand = FALSE, xlim=c(min(data$Long), max(data$Long)), ylim=c(min(data$Lat), max(data$Lat)))+
      xlab("Longitude")+ ylab("Latitude") + ggtitle(label="Mean")
  } else {
    ggplot(data = data) +
      geom_raster(aes(x = {{x}}, y = {{y}}, fill = {{fill}})) +
      facet_grid(rows = vars(Wheather), switch=c('y'), labeller=as_labeller(c(`A`="Niño",`B`="Neutral", `C`="Niña")))+
      scale_fill_stepsn(n.breaks = 9, colours = viridis::magma(9),name=" Yield"~(kg~ha^{-1}), limits = limits)+ theme_bw()+
      theme(legend.position = "right")+ 
      geom_sf(data=shp, fill=NA, color="white", linewidth=0.5)+
      coord_sf(expand = FALSE, xlim=c(min(data$Long), max(data$Long)), ylim=c(min(data$Lat), max(data$Lat)))+
      xlab("Longitude")+ ylab("Latitude") + ggtitle(label="Standard Deviation")
  }
}

# 3. Aggregated DSSAT output  -------------------------------------------
#(To check if function from L. Leroux is more efficient)
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param season when data is needed for more than one season, this needs to be provided to be used in the file name
#' @param varietyids ids of the varieties based on the cultivar file of DSSAT (column @VAR# in the cultivar file and parameter INGENO in the experimental file *.**X)
#' @param zone_folder When TRUE the output folders are organized by administrative level 1.
#' @param level2_foler When TRUE the output folders are organized by administrative level 2 (has to be part of the administrative level 1 or "zone" of the country) 
#'        for the specific location the experimental file is created
#'        
#' @return merged results from DSSAT in RDS format
#'
#' @examples merge_DSSAT_output(country="Rwanda", useCaseName="RAB",Crop="Maize",varietyids=c("890011","890012"), zone_folder=T, level2_folder=F)

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/merge_DSSAT_output.R")

# 4. Get ONI Index  -------------------------------------------

  #' @description Function that will classify each DSSAT simulation based on ONI index
  #' @param country country name
  #' @param useCaseName use case name
  #' @param Crop targeted crop
  #' @param AOI TRUE if the data is required for target area, and FALSE if it is for trial sites, default =TRUE
  #' @param Season integer, cropping season concerned, default = 1 
  #' @param Plot, provide somes plots of the output, default = TRUE
  #' @param short_variety variety ID with short growing period duration
  #' @param medium_variety variety ID with medium growing period duration 
  #' @param long_variety variety ID with long growing period duration
  #'
  #' @return A table with the aggregated DSSAT simulation classified based on ONI index - Output table is written out in "~/agwise-potentialyield/dataops/potentialyield/Data/useCaseName/Crop/result/DSSAT/Extent/useCase_country_useCaseName_crop_Extent_season_X_ONI.RDS")
  #'
  #' @examples get_ONI(country= "Kenya", useCaseName = "KALRO", Crop="Maize", AOI=T, Season=1, Plot=TRUE, short_variety="900111", medium_variety="900112",long_variety="900113")
  #' 

get_ONI <- function(country, useCaseName, Crop, AOI=TRUE, season, Plot=TRUE, short_variety, medium_variety, long_variety){

  ## 4.1. Creating a directory to store output table and graphics ####
  if (AOI == TRUE){
    pathOut <- paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName,"/",Crop,"/result/DSSAT/AOI/", sep="")
  } else {
    pathOut <- paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName,"/",Crop,"/result/DSSAT/fieldData/",  sep="")
  }
  
  
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }
  
  ## 4.2. Read the input data ####
  ### 4.2.1. Get the ONI data ####
  # Check if the ONI table is already available
  
  url <- "https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt"
  destfile <- "/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/ONI/"
  file_name <- "oni.txt"
  
  # Download it if doesn't exist
  if (!file.exists(paste(destfile, file_name, sep = ""))){
    download.file(url, paste(destfile, file_name, sep = ""), mode="wb")
  } else {
  # Update the ONI version with the last version
    unlink("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/ONI/oni.txt")
    download.file(url, paste(destfile, file_name, sep = ""), mode="wb")
  }
  
  # Open the ONI data
  oni <- read_table(paste0(destfile,file_name))
  
  # Reshape the ONI data
  month_mapping <- c(
    'DJF'= 'January',
    'JFM'= 'February',
    'FMA'= 'March',
    'MAM'= 'April',
    'AMJ'= 'May',
    'MJJ'= 'June',
    'JJA'= 'July',
    'JAS'= 'August',
    'ASO'= 'September',
    'SON'= 'October',
    'OND'= 'November',
    'NDJ'= 'December'
  )
  oni$month <- month_mapping[oni$SEAS]
  oni$date <- as.Date(paste0(oni$YR, "-", oni$month, "-01"), format="%Y-%B-%d")
  
  ### 4.2.2. Get the aggregated DSSAT output and reshape the data ####
  # Open the aggregated DSSAT output
  if(AOI ==TRUE){
   dssat_path <- paste0(pathOut,"useCase_", country, "_" ,useCaseName,"_",Crop,"_AOI_season_",season,".RDS", sep="")
  }else{
  dssat_path <- paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_fieldData_season_",season,".RDS", sep="")
  }
  
   # Check if the object all_results with newly aggregated data is still in memory to be put into the path if not read the saved object        
  #ifelse(file.exists(dssat_path)==FALSE, dssat <- all_results, dssat <- readRDS(dssat_path))
  dssat <- readRDS(dssat_path)

  ## 4.3. Get the median ONI over the cropping season for aggregated DSSAT output ####
  # Ranges of date of analysis
  date_ranges <- data.frame (
    initial_date = dssat$PDAT, # Planting Date
    final_date = dssat$HDAT    # Harvesting Date
  )
																	 
  date_ranges <- unique(date_ranges)

  # Compute the median ONI over the cropping season #(takes alot of time)
 # med_oni <- pmap_dfr(date_ranges, get_median_variable, variable="ANOM", data=oni)

 # # Update the aggregated DSSAT output
 # dssat_oni <- bind_cols(dssat,med_oni)
 
  
  medoni <- foreach::foreach(i=1:nrow(date_ranges)) %dopar% {
    filtered_data <- oni %>%
      filter(date >= as.Date(date_ranges$initial_date[i]) & date <= as.Date(date_ranges$final_date[i]))
    vars <- as.numeric(filtered_data$ANOM)
    if(i%%10000==0){
      print(i)}
    median(vars)}
  medoni <- do.call(rbind, medoni)

  # Convert medoni to a dataframe with a specific column name
  medoni_df <- tibble(med_variable = medoni)
  
  date_ranges_oni <- bind_cols(date_ranges,medoni_df)
  
  # Merge with DSSAT output
  dssat_oni <- merge(dssat, date_ranges_oni, by.x = c('PDAT', 'HDAT'), by.y = c('initial_date', 'final_date'))
  #colnames(dssat_oni) <- c(colnames(dssat_oni)[-27],'med_variable')
  
  ## 4.4. Classify tbe ONI into three classes ####
  # med ONI > 0.5, Nino, med ONI < -0.5 Nina, -0.5 > ONI > 0.5 Neutral 

  dssat_oni$ENSO <- ifelse(dssat_oni$med_variable > 0.5,"Niño",
                             ifelse(dssat_oni$med_variable< -0.5,"Niña", "Neutral"))
  
  #Change the name of the varieties by their growing duration
  # Apply conditional transformation using case_when
  dssat_oni <- dssat_oni %>%
    mutate(Variety = case_when(
      Variety ==short_variety ~ "Short",
      Variety ==medium_variety ~ "Medium",
      Variety ==long_variety ~ "Long"
    ))
															   
											
  # Save the aggregated DSSAT output with ONI information
  if(AOI ==TRUE){
    saveRDS(dssat_oni,paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_AOI_season_",Season,"_ONI.RDS" ))
  }else{
    saveRDS(dssat_oni,paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_fieldData_season_",Season,"_ONI.RDS" ))
  }
  
  ## 4.5. Basic plots ####
  if (Plot == TRUE){
    ### 4.5.1. Global plot ####
    #dssat_oni <- na.omit(dssat_oni)
    # Define a manual scale
    val <- c("Neutral" = "gold",
             "Niño" = "tomato1",
             "Niña" = "royalblue2")
						 
  # Meand and SEM plot
  pd <- position_dodge(0.2)
  dssat_oni %>%
    summarySE(measurevar="HWAH", groupvars=c("Variety","TNAM", "ENSO"))%>%
  ggplot(aes(x = TNAM, 
             y = HWAH, 
             group=ENSO, 
             color=ENSO)) +
    geom_point(position = pd, size = 3) +
    geom_line(position = pd,linewidth = 1) +
    geom_errorbar(aes(ymin = HWAH - se, ymax = HWAH + se), width = .1, position= pd)+
    theme_bw()+
    scale_color_manual(values=val)+ theme(axis.text.x =element_text(angle=45, hjust=1))+
    ylab(expression(bold('Yield'~(kg~ha^{-1})))) + xlab(expression(bold("Planting time")))
  if(AOI==TRUE){
    ggsave(paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_AOI_season_",Season,"_ONI_DotPlot_Global.pdf"))
  }else{
    ggsave(paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_fieldData_season_",Season,"_ONI_DotPlot_Global.pdf"))
  }
  

  # Heat map
  p1 <- dssat_oni %>%
    summarySE(measurevar="HWAH", groupvars=c("Variety","TNAM", "ENSO"))%>%
  ggplot(aes(x=TNAM, y=ENSO, fill=HWAH))+
     geom_tile(color="white", linewidth=0.1)+
     theme_bw()+
     theme(axis.text.x =element_text(angle=45, hjust=1))+
     scale_fill_viridis_c(name="Yield"~(kg~ha^{-1}))+
     coord_equal()+
    ylab(expression(bold("ENSO")))+xlab(expression(bold("Planting time")))+
    ggtitle("Mean yield")
  
 p2 <-  dssat_oni %>%
   summarySE(measurevar="HWAH", groupvars=c("Variety","TNAM", "ENSO"))%>%
    ggplot(aes(x=TNAM, y=ENSO, fill=sd))+
    geom_tile(color="white", linewidth=0.1)+
    theme_bw()+
    theme(axis.text.x =element_text(angle=45, hjust=1))+
    scale_fill_viridis_c(name="Yield"~(kg~ha^{-1}))+
    coord_equal()+
    ylab(expression(bold("ENSO")))+xlab(expression(bold("Planting time")))+
    ggtitle("Standard Deviation")
 p1+p2+plot_layout(ncol = 1) 
 
 if(AOI==TRUE){
   ggsave(paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_AOI_season_",Season,"_ONI_HeatMap_Global.pdf"))
 }else{
   ggsave(paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_fieldData_season_",Season,"_ONI_HeatMap_Global.pdf"))
 }
 
 
 ### 4.5.2. Plot by variety ####
 # Mean and SEM plot
 pd <- position_dodge(0.2)
 dssat_oni %>%
   summarySE(measurevar="HWAH", groupvars=c("Variety","TNAM", "ENSO"))%>%
   ggplot(aes(x = TNAM, 
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

 if(AOI==TRUE){
   ggsave(paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_AOI_season_",Season,"_ONI_DotPlot_Variety.pdf"))
 }else{
   ggsave(paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_fieldData_season_",Season,"_ONI_DotPlot_Variety.pdf"))
 }
 
 
 # Heat map
 p1 <- dssat_oni %>%
   summarySE(measurevar="HWAH", groupvars=c("Variety","TNAM", "ENSO"))%>%
   ggplot(aes(x=TNAM, y=ENSO, fill=HWAH))+
   facet_grid(rows = vars(Variety))+
   geom_tile(color="white", linewidth=0.1)+
   theme_bw()+
   theme(axis.text.x =element_text(angle=45, hjust=1))+
   scale_fill_viridis_c(name="Yield"~(kg~ha^{-1}))+
   coord_equal()+
   ylab(expression(bold("ENSO")))+xlab(expression(bold("Planting time")))+
   ggtitle("Mean yield")
 
 p2 <-  dssat_oni %>%
   summarySE(measurevar="HWAH", groupvars=c("Variety","TNAM", "ENSO"))%>%
   ggplot(aes(x=TNAM, y=ENSO, fill=sd))+
   facet_grid(rows = vars(Variety))+
   geom_tile(color="white", linewidth=0.1)+
   theme_bw()+
   theme(axis.text.x =element_text(angle=45, hjust=1))+
   scale_fill_viridis_c(name="Yield"~(kg~ha^{-1}))+
   coord_equal()+
   ylab(expression(bold("ENSO")))+xlab(expression(bold("Planting time")))+
   ggtitle("Standard Deviation")
 p1+p2+plot_layout(ncol = 2) 
 
 if(AOI==TRUE){
   ggsave(paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_AOI_season_",Season,"_ONI_HeatMap_Variety.pdf"))
 }else{
   ggsave(paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_fieldData_season_",Season,"_ONI_HeatMap_Variety.pdf"))
 }
 
 ## 4.6. Maps ####
 
 # Read the relevant shape file from gdam to be used to crop the global data
 countryShp <- geodata::gadm(country, level = 1, path=paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName,"/",Crop,"/result/DSSAT/", sep=""))
 country_sf <- sf::st_as_sf(countryShp)
 
							 
  
 ### 4.6.1. Global Map ####
 dssat_oni.g <- dssat_oni %>%
   summarySE(measurevar="HWAH", groupvars=c("Lat","Long", "ENSO"))
 
 # Scale limits
 min.mean <- min(dssat_oni.g$HWAH)
 max.mean <- max(dssat_oni.g$HWAH)
 
 min.sd <- min(dssat_oni.g$sd)
 max.sd <- max(dssat_oni.g$sd)
 
 ## Neutral
 dssat.neutral.g <- subset(dssat_oni.g, dssat_oni.g$ENSO %in% 'Neutral', select = -c(ENSO, N))
dssat.neutral.g <- na.omit(dssat.neutral.g[,c(2,1,3,4,5,6)])
 
 ## Nino
 dssat.nino.g <- subset(dssat_oni.g, dssat_oni.g$ENSO %in% 'Niño', select = -c(ENSO, N))
 dssat.nino.g <- na.omit(dssat.nino.g[,c(2,1,3,4,5,6)])
 
 ## Nina
 dssat.nina.g <- subset(dssat_oni.g, dssat_oni.g$ENSO %in% 'Niña', select = -c(ENSO, N))
 dssat.nina.g <- na.omit(dssat.nina.g[,c(2,1,3,4,5,6)])
 
 ## Assembling
 dssat.g.mean <- dssat.neutral.g
 dssat.g.mean$Wheather <- "B"
 dssat.nino.g$Wheather <- 'A'
 dssat.nina.g$Wheather <- 'C'
 dssat.g.mean <- rbind(dssat.g.mean, dssat.nino.g)
 dssat.g.mean <- rbind(dssat.g.mean, dssat.nina.g)
 
 mean.g <-oni_map(data=dssat.g.mean, x=Long, y=Lat,shp=country_sf, fill=HWAH, HWAH= TRUE,limits=c(min.mean, max.mean))
 sd.g <-oni_map(data=dssat.g.mean, x=Long, y=Lat,shp=country_sf, fill=sd, HWAH= FALSE, limits=c(min.sd, max.sd))
 
 ass <- plot_grid(mean.g, sd.g)
 # Final Layout
 title <- ggdraw() + draw_label(
     "Water limited yield according to main weather patterns:",
     fontface = 'bold',
     x = 0,
     hjust = 0
   ) +
   draw_label(
     paste(country, "-", useCaseName, " use case", "-", Crop),
     fontface = 'bold',
     x = 0,
     y=0.15,
     hjust = 0
   )+
   theme(
     # add margin on the left of the drawing canvas,
     # so title is aligned with left edge of first plot
     plot.margin = margin(0, 0, 0, 7)
   )
 
plot_grid(
   title, ass,
   ncol = 1,
   # rel_heights values control vertical title margins
   rel_heights = c(0.1, 1)
 )

if(AOI==TRUE){
  ggsave(paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_AOI_season_",Season,"_ONI_Maps_Global.pdf"), dpi=300, width = 8, height=6.89, units=c("in"))
}else{
  ggsave(paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_fieldData_season_",Season,"_ONI_Maps_Global.pdf"), dpi=300, width = 8, height=6.89, units=c("in"))
}

##################################################
### Find optimum planting dates and plot them ####
##################################################

dssat_oni$year<-year(dssat_oni$PDAT)
dssat_oni$date<-dssat_oni$PDAT
dssat_oni$doy<-strftime(dssat_oni$PDAT,format="%j")
dssat_oni$ydoy<-strftime(dssat_oni$PDAT,format="%y%j")
dssat_oni<-as.data.frame(dssat_oni)

gps <- readRDS(paste0("~/agwise-datacuration/dataops/datacuration/Data/useCase_", country, "_",useCaseName,"/", Crop, "/result/AOI_GPS.RDS"))


pyd <- merge(gps, dssat_oni,by.x=c("lon","lat"), by.y=c("Long","Lat"))

pyd$ONI<-pyd$med_variable
pyd$NAME_0<-pyd$country
pyd$yield <-pyd$HWAH
pyd$prov <-pyd$Loc

pyd_select<-pyd[,c("NAME_0", "NAME_1","NAME_2","lon","lat","TRNO","TNAM","PDAT",
                   "yield","Variety","prov","ONI","ENSO","year","date","doy",
                   "ydoy")]


summary_pyd<-pyd_select %>%
  dplyr::group_by(lon,lat,Variety,ENSO,TRNO,TNAM,doy) %>%
  dplyr::summarize(median = median(yield, na.rm = TRUE))



#get only top 5 optimum dates per ENSO, variety, and location
max_median_summary_poptions <- summary_pyd %>%  
  group_by(lon,lat,Variety,ENSO) %>% 
  slice_max(median,n=5) %>%
  select(TRNO,TNAM,median, doy) %>%
  arrange(ENSO,Variety, desc(median))%>%
  filter(!is.na(ENSO))

# Save the top optimum 5 planting dates per ENSO per variety per location
if(AOI ==TRUE){
  saveRDS(max_median_summary_poptions,paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_AOI_season_",Season,"_optimum_planting5.RDS" ))
}else{
  saveRDS(max_median_summary_poptions,paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_fieldData_season_",Season,"_optimum_planting5.RDS" ))
}
#get only one optimum date
max_median_summary <- summary_pyd %>%  
  group_by(lon,lat,Variety,ENSO) %>% 
  slice_max(median) %>%
  select(TRNO,TNAM,median, doy) %>%
  arrange(desc(median))%>%
  filter(!is.na(ENSO))

# Save the top optimum planting date per ENSO per variety per location
if(AOI ==TRUE){
  saveRDS(max_median_summary,paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_AOI_season_",Season,"_optimum_planting.RDS" ))
}else{
  saveRDS(max_median_summary,paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_fieldData_season_",Season,"_optimum_planting.RDS" ))
}

year <- 2023
max_median_summary$doyy <- as.Date(as.numeric(as.character(max_median_summary$doy))-1, origin = paste0(year, "-01-01"))
max_median_summary$Opt_date <- format(max_median_summary$doyy, "%d-%b")

max_median_summary$TRNO <-factor(max_median_summary$TRNO)

max_median_summary$Opt_date <- factor(max_median_summary$Opt_date, 
                                      levels = unique(max_median_summary$Opt_date[order(max_median_summary$TRNO)]))

max_median_summary <- na.omit(max_median_summary)
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
  geom_sf(data=country_sf, fill=NA, color="white", linewidth=0.5)+
  coord_sf(xlim=c(min(max_median_summary$lon), max(max_median_summary$lon)), 
           ylim=c(min(max_median_summary$lat), max(max_median_summary$lat)))+
  xlab("Longitude")+ ylab("Latitude")
#d
if(AOI ==TRUE){
  ggsave(plot=d,paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_AOI_season_",Season,"_OptPlanting_ONI.png"), width = 12, height = 12)
  ggsave(plot=d,paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_AOI_season_",Season,"_OptPlanting_ONI.pdf"), width = 12, height = 12)
  }else{
  ggsave(plot=d,paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_fieldData_season_",Season,"_OptPlanting_ONI.png" ), width = 12, height = 12)
  ggsave(plot=d,paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_fieldData_season_",Season,"_OptPlanting_ONI.pdf" ), width = 12, height = 12)
  }

 
  }
}

# country="Kenya"
# useCaseName="KALRO"
# Crop = "Maize"
# Extent = "AOI"
# Season = 1
# Plot = TRUE
# 
# 
# get_ONI(country, useCaseName, Crop, Extent, Season, Plot)
# merge_DSSAT_output(country, useCaseName, Crop, Extent, Season)
  # Meand and SEM plot

 

