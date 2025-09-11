#Load the necessary packages needed to run the simulation line 2 to 8
my_packages <- c("spdep", "raster", "plyr", "ggplot2", "sf",
                 "dplyr", "cowplot","readxl", "apsimx", "gtools", "foreach","doParallel",
                 "ranger","geodata","terra")

list.of.packages <- my_packages
new.packages <- list.of.packages[!(my_packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(my_packages, require, character.only = TRUE)

#Downloads the soil and prepare the weather data on run once will generate climate and soil files in APSIMx format
wkdir <- "~/agwise-potentialyield/dataops/potentialyield/Script/generic/APSIM"
dataDir <- paste0(wkdir,"/RwandaData/")
sourceDir <- paste0(wkdir,"/Scripts/")


source(paste0(wkdir,"/01_1readGeo_CM_zone_APSIM.R"))

country <- "Rwanda"
useCaseName <- "RAB"
Crop <-"Maize"
AOI = TRUE
season=1
zone <- "Amajyaruguru"
level2=NA
varietyid<- "Early"
pathIn_zone = T
Depth = c(5,15,30,60,100,200)


readGeo_CM_zone_APSIM(country, useCaseName, Crop, AOI, season, zone,level2,varietyid,pathIn_zone, Depth)
  






setwd(sourceDir)
source('02_aspimSpatialFactorial.R')
scfl <- paste0(wkdir,"/Maize_Factorial/")
wkdirprj <- paste0(wkdir,"/project/")

SimAugSep <- apsimSpatialFactorial(scfl = scfl,
                                  my_list_clm = my_list_clm,
                                  wkdir =wkdirprj, 
                                  crop = "MaizeFactorialAugSep.apsimx", 
                                  clck = c("1981-01-01T00:00:00", "2020-12-31T00:00:00"),
                                  variety = "Early",
                                  rep1 ="[Maize].Grain.Total.Wt*10 as Yield",
                                  rep2 ="[Maize].SowingDate",
                                  ppln = 5.3)

## sourcing function to run the spatialized apsim
#This function runs all the 808 files and generates a big list of 808 files with the results for each point
#An example of how to run this
# resultsAugSep<-my_list_sim(crop = "MaizeFactorialAugSep.apsimx", #Name of the apsim.x files each with unique climate and soil
#                            my_list_clm = my_list_clm, #Same list of climate files generated in line 11
#                            extd.dir = "/home/jovyan/agwise/project", #Name of folder where you have stored the unique 808 apsimx files
#                            stn = stn, #Name of the excel file with station cordinates
#                            my_list_soil = my_list_sol[[1]]) #Same list of soil files generated in line 11

setwd(sourceDir)
source('03_RunSim.R')

resultsAugSep<-my_list_sim(crop = "MaizeFactorialAugSep.apsimx",
                           my_list_clm = my_list_clm, 
                           extd.dir = wkdirprj, 
                           stn = stn,
                           my_list_soil = my_list_sol)
                           
                           # my_list_soil = my_list_sol)
                          # my_list_soil = my_list_sol[[1]])

#We save the list produces in line 60 under the file path located below
resultfile <- paste0(wkdir,"/Maize_Results/season1_outputLA/resultsAugSep.RData") 
save(resultsAugSep, file=resultfile)

##Here is the post processing Section extra function to help the user visualise their results
load(file=resultfile)

##Sourcing Script to produce one data frame with all the results of all the 808 files; Also visualise the points in a map.
#An example how to run the function


setwd(sourceDir)
source('04_ApsimPlotFactorial.R')

PlantingDatesAugSep<-apsim.plots(stn = stn, #Dataframe with the station coordinates
                                 results=resultsAugSep, #The list of results from line 60 that will be merged into one
                                 b= "RWANDA", #The name of the country shapefile you want
                                 wkdir=  wkdirprj) 




#We save the big dataframe produced in line 84 under the file path located below
outputfile <- paste0(wkdir,"/OutputData/APSIM_MZ_SHT_S2.RData") 
save(PlantingDatesAugSep, file=outputfile)



