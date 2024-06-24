source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/Testing/readGeo_CM_V2.R")

#################################################################################################################
## Create soil and weather data in DSSAT format for trial data
#################################################################################################################

#geoData <- readGeo_CM(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Soybean", AOI = FALSE,  Planting_month_date = NULL,Harvest_month_date=NULL,season=NULL,plantingWindow=NULL)

#################################################################################################################
## Create soil and weather data in DSSAT format for AOI data
#################################################################################################################
country <- "Kenya"
countryShp <- geodata::gadm(country, level = 2, path='.')
prov <- unique(countryShp$NAME_1)

# prov <- c("Baringo","Bomet","Bungoma","Busia","Elgeyo-Marakwet", "Embu", "Homa Bay",
#                "Kajiado","Kakamega","Kericho","Kiambu","Kilifi","Kirinyaga","Kisii", "Kisumu","Kitui",
#                "Kwale","Laikipia","Lamu","Machakos","Makueni", "Meru","Migori","Murang'a","Nakuru","Nandi",
#                "Narok","Nyamira","Nyandarua","Nyeri","Samburu","Siaya","Taita Taveta","Tana River","Tharaka-Nithi","Trans Nzoia",
#                "Uasin Gishu","Vihiga","West Pokot","Mombasa")

prov <- c("Garissa","Nairobi","Isiolo","Wajir","Turkana","Marsabit", "Mandera") 


for (i in 1:length(prov)){
  geoData_AOI <- readGeo_CM(country="Kenya",  useCaseName = "KALRO", Crop = "Maize", AOI = TRUE, season=1, Province = prov[i])}



