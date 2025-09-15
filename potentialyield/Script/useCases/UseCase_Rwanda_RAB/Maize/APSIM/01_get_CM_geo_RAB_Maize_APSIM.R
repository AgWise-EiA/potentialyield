#################################################################################################################
## source "get_rain_temp_summary.R" function and get weather data 
#################################################################################################################
source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/APSIM/01_1readGeo_CM_zone_APSIM.R")


#################################################################################################################
## Create soil and weather data in APSIM format for AOI data
#################################################################################################################
country <- "Rwanda"
useCaseName = "RAB"
Crop = "Maize"
varietyid <- "Early"
countryShp <- geodata::gadm(country, level = 2, path='.')
prov <- unique(countryShp$NAME_1)

path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", 
                                            country, "_",useCaseName, "/", Crop, "/transform/APSIM/AOI/", sep="")

log_file <- paste(path.to.extdata,"progress_log_readGeo_CM_APSIM.txt",sep='/')

if (file.exists(log_file)) {
  file.remove(log_file)
}

start_time <- Sys.time()
for (i in 1:length(prov)){

  geoData_AOI <- readGeo_CM_zone_APSIM(country=country,  useCaseName = useCaseName, Crop = Crop, AOI = TRUE, season=1, zone=prov[i],
                                 level2=NA,varietyid,pathIn_zone = T, Depth = c(5,15,30,60,100,200))
  message <- paste("Province finished:", i,Sys.time())
  cat(message, "\n", file = log_file, append = TRUE)
}
end_time <- Sys.time()
duration <- end_time - start_time
cat(duration, "\n", file = log_file, append = TRUE)













