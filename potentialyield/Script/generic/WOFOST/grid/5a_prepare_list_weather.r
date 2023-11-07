#create the data.frame with weather data for location i
## more info: https://reagro.org/methods/explanatory/wofost/weather.html

#current site, year and siteyear
siteh<-xy.df[h,"ID"]
yearh<-xy.df[h,"year"]
siteyearh<-xy.df[h,"siteyear"]

#compile weather data for current site and season
weath<-data.frame(date=d)
weath$date<-as.Date(weath$date, dformatworking)
for (k in weathervars){
  v<-get(k)[,siteyearh]
  weath<-cbind(weath, v)
  names(weath)[names(weath)=="v"]<-k
}

#subset weather data to include only dates between planting and harvest
sel<-complete.cases(weath)
weath<-weath[sel,]

#convert rh to vapr (could perhaps be improved by conmputing air pressure from to elevation)
weath$tmean=(weath$tmin +weath$tmax)/2
weath$vapr<-1000*weath$rh*0.01*esat(TdegC=weath$tmean, Pa = 101)

#convert srad from MJ m-2 day-1 to kJ m-2 day-1
weath$srad=weath$srad*1000

#keep only relevant columns
wofostweathervars<-c("date", "srad", "tmin", "tmax", "vapr", "wind", "prec")
weath<-weath[,wofostweathervars]

