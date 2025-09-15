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
expfile_name = "MaizeFactorialAugSep.apsimx" 
clck = c("1981-01-01T00:00:00", "2020-12-31T00:00:00")
varietyid = "Early"
rep1 ="[Maize].Grain.Total.Wt*10 as Yield"
rep2 ="[Maize].SowingDate"
ppln = 5.3
level2=NA
AOI = TRUE
pathIn_zone = T


start_time <- Sys.time()
for (i in 1:length(prov)){

  apsimSpatialFactorial(country=country,  useCaseName = useCaseName, Crop = Crop, AOI = AOI,
                        season=1,zone=prov[i],level2=level2,pathIn_zone=pathIn_zone,expfile_name=expfile_name,
                        clck=clck,varietyid=varietyid,ppln=ppln,rep1=rep1,rep2=rep2)
    

}
end_time <- Sys.time()
duration <- end_time - start_time














