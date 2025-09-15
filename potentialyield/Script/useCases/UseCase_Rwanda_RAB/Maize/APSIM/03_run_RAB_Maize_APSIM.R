#################################################################################################################
## source "get_rain_temp_summary.R" function and get weather data 
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/APSIM/03_RunSim.R")


#################################################################################################################
## Run the APSIM experiment
#################################################################################################################
country <- "Rwanda"
useCaseName = "RAB"
Crop = "Maize"
countryShp <- geodata::gadm(country, level = 2, path='.')
prov <- unique(countryShp$NAME_1)
expfile_name = "MaizeFactorialAugSep.apsimx" 
varietyid = "Early"
level2=NA
AOI = TRUE



start_time <- Sys.time()
for (i in 1:length(prov)){

  apsim.exec(country=country,  useCaseName = useCaseName, Crop = Crop, AOI = AOI,
             expfile_name=expfile_name,varietyid=varietyid, zone=prov[i], level2=level2)

}
end_time <- Sys.time()
duration <- end_time - start_time














