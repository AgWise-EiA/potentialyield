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

path.to.varietyid <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", 
                         country, "_",useCaseName, "/", Crop, "/transform/APSIM/AOI/", varietyid, sep="")

# This file will show soil parameters that had issues and were corrected
soil_log_path <- paste(path.to.varietyid,"soil_issues_log.txt",sep='/')
soil_log_file <- file.path(soil_log_path)
if (file.exists(soil_log_file)) file.remove(soil_log_file)

start_time <- Sys.time()
for (i in 1:length(prov)){
  
  apsim.exec(country=country,  useCaseName = useCaseName, Crop = Crop, AOI = AOI,
             expfile_name=expfile_name,varietyid=varietyid, zone=prov[i], level2=level2)

}
end_time <- Sys.time()
duration <- end_time - start_time
cat(duration)


