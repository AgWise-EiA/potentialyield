#Load the necessary packages needed to run the simulation line 2 to 8
my_packages <- c("spdep", "rgdal", "maptools", "raster", "plyr", "ggplot2", "rgdal",
                 "dplyr", "cowplot","readxl", "apsimx", "gtools", "foreach","doParallel",
                 "ranger")
list.of.packages <- my_packages
new.packages <- list.of.packages[!(my_packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(my_packages, require, character.only = TRUE)

#Downloads the soil and prepare the weather data on run once will generate climate and soil files in APSIMx format
source("D:/AgWise_APSIM/Scripts/01_GetSoilandWeather.R")

#Read the coordinates data and change the column names line 14 to 15
stn<- read.csv("D:/AgWise_APSIM/RwandaData/coordinates_Rwanda.csv")
names(stn)<- c("Longitude", 'Latitude', "Location")
stn<-stn[1:5,]
## Load list of met files generated in line 11
setwd("D:/AgWise_APSIM/")
load(file="my_list_clm.RData")

#Load list of soil files generated in line 11
load(file="my_list_sol.RData")

## Involves sourcing function to create spatialize apsim and also editing it example of what the function does
#This will write 808 and apsimx files with a unique climate and soil file
setwd("D:/AgWise_APSIM/Scripts/")
source('02_aspimSpatialFactorial.R')

SimAugSep <- apsimSpatialFactorial(scfl ="D:/AgWise_APSIM/Maize_Factorial/",
                                  my_list_clm = my_list_clm,
                                  wkdir ="D:/AgWise_APSIM/project/", 
                                  crop = "MaizeFactorialAugSep.apsimx", 
                                  clck = c("1981-01-01T00:00:00", "2020-12-31T00:00:00"),
                                  variety = "Early",
                                  rep1 ="[Maize].Grain.Total.Wt*10 as Yield",
                                  rep2 ="[Maize].SowingDate",
                                  ppln = 5.3)

## sourcing function to run the spatialized apsim
#This function runs all the 808 files and generates a big list of 808 files with the results for each point
#An example of how to run this

setwd("D:/AgWise_APSIM/Scripts/")
source('03_RunSim.R')

resultsAugSep<-my_list_sim(crop = "MaizeFactorialAugSep.apsimx",
                           my_list_clm = my_list_clm, 
                           extd.dir = "D:/AgWise_APSIM/project", 
                           stn = stn,
                           my_list_soil = my_list_sol)
                           

#We save the list produces in line 60 under the file path located below
save(resultsAugSep, file="D:/AgWise_APSIM/Maize_Results/season1_outputLA/resultsAugSep.RData")

##Here is the post processing Section extra function to help the user visualise their results
load(file="D:/AgWise_APSIM/Maize_Results/season1_outputLA/resultsAugSep.RData")

##Sourcing Script to produce one data frame with all the results of all the 808 files; Also visualise the points in a map.
#An example how to run the function
library(geodata)
library(terra)
library(sf)

setwd("D:/AgWise_APSIM/Scripts/")
source('04_ApsimPlotFactorial.R')

PlantingDatesAugSep<-apsim.plots(stn = stn, #Dataframe with the station coordinates
                                 results=resultsAugSep, #The list of results from line 60 that will be merged into one
                                 b= "RWANDA", #The name of the country shapefile you want
                                 wkdir= "D:/AgWise_APSIM/project") #Path to store the large dataframe

setwd("D:/AgWise_APSIM/Scripts/")
source('04_ApsimPlotFactorial.R')

PlantingDatesFebMar<-apsim.plots(stn = stn,
                                 results=resultsAugSep, 
                                 b= "RWANDA",
                                 wkdir= "D:/AgWise_APSIM/project") #Make a new directory, project.

#We save the big dataframe produced in line 84 under the file path located below
save(PlantingDatesAugSep, file="D:/AgWise_APSIM/OutputData/APSIM_MZ_SHT_S2.RData")



