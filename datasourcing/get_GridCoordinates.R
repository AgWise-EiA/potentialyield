

getCoordinates <- function(country){
  
  ## get country abbreviation to used in gdam function
  countryCC <- countrycode(country, origin = 'country.name', destination = 'iso3c')
  
  
  ## read the relevant shape file from gdam to be used to crop the global data
  level3 <- geodata::gadm(countryCC, level = 3, path='.')
 
  
  xmin <- ext(level3)[1]
  xmax <- ext(level3)[2]
  ymin <- ext(level3)[3]
  ymax <- ext(level3)[4]
  
  ## define a rectangular area that covers the whole study area
  lon_coors <- seq(xmin - 0.1, xmax + 0.1, by=0.05)
  
  if(ymin > 0 & ymax > 0){
    lat_coors <- seq(ymin - 0.1, ymax + 0.1, by=0.05)
  }else if (ymin < 0 & ymax > 0){
    lat_coors_neg <- seq(ymin - 0.1, 0, by=0.05)
    lat_coors_pos <- seq(0, ymax + 0.1, by=0.05)
    lat_coors <- c(lat_coors_neg, lat_coors_pos)
  }else if (ymin < 0 & ymax < 0){
    lat_coors <- seq(ymin - 0.1, ymax - 0.1, by=0.05)
  }
  
  
  rect_coord <- as.data.frame(expand.grid(Longitude = round(lon_coors, digits = 3), Latitude = round(lat_coors, digits = 3)))
  rect_coord$x <- floor(rect_coord$Longitude*10)/10 + ifelse(rect_coord$Longitude - (floor(rect_coord$Longitude*10)/10) < 0.05, 0.025, 0.075)
  rect_coord$y <- floor(rect_coord$Latitude*10)/10 + ifelse(abs(rect_coord$Latitude)-(floor(abs(rect_coord$Latitude)*10)/10) < 0.025, 0.025, 0.075)
  rect_coord <- unique(rect_coord[,c("x", "y")])
  
  State_LGA <- as.data.frame(raster::extract(level3, rect_coord))
  State_LGA$longitude <- rect_coord$x
  State_LGA$latitude <- rect_coord$y
  State_LGA$country <- country
  
  State_LGA <- unique(State_LGA[, c("country", "NAME_1", "NAME_2", "NAME_3", "longitude", "latitude")])
  State_LGA <- State_LGA[!is.na(State_LGA$NAME_1), ]
  
  return(State_LGA)
}