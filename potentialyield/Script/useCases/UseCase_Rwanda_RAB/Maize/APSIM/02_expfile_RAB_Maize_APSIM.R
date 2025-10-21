#################################################################################################################
## source "get_rain_temp_summary.R" function and get weather data 
#################################################################################################################
source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/APSIM/02_apsimSpatialFactorial.R")


#################################################################################################################
## Create soil and weather data in APSIM format for AOI data
#################################################################################################################
country <- "Rwanda"
useCaseName = "RAB"
Crop = "Maize"
countryShp <- geodata::gadm(country, level = 2, path='.')
prov <- unique(countryShp$NAME_1)
expfile_name <- "MaizeFactorialAugSep.apsimx"
fix_crop_or_soil_parm <- "soil"

clck = c("1981-01-01T00:00:00", "2020-12-31T00:00:00")
varietyid = "Early"
rep <- c("[Maize].Grain.Total.Wt*10 as Yield",
         "[Maize].SowingDate")

level2=NA
AOI = TRUE
pathIn_zone = T


start_time <- Sys.time()
for (i in 1:length(prov)){
  apsimSpatialFactorial(country=country, useCaseName = useCaseName, Crop = Crop, AOI = AOI,
                        season=1,zone=prov[i],level2=level2,pathIn_zone=pathIn_zone,expfile_name=expfile_name,
                        clck=clck,varietyid=varietyid,rep=rep,fix_crop_or_soil_parm=fix_crop_or_soil_parm)
    

}
end_time <- Sys.time()
duration <- end_time - start_time














