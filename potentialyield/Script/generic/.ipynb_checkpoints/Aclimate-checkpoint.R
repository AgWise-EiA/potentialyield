
#https://github.com/CIAT-DAPA/aclimaterapi
# devtools::install_github("CIAT-DAPA/aclimaterapi")

library("aclimaterapi")
url_root = "https://webapi.aclimate.org/api/"
AclimateCountries = get_geographic_country(url_root)
print(AclimateCountries)
weatherStations = get_geographic(url_root, country_id= "641c820e4fb2a6438cc670e7")
print(head(weatherStations))
head(weatherStations[weatherStations$state_name == "Zone 1", ])
soilCultivars = get_agronomy(url_root)
print(soilCultivars)
stations=c("64b14c7c4a86115b9ae68767") ## ws_id Akaki - Kalit
obj_f = get_forecast_climate(url_root, stations)
print(obj_f$probabilities)
print(obj_f$performance)
print(obj_f$scenarios)
df = get_forecast_crop(url_root, stations)
head(df)
