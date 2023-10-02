#create the data.frame with weather data for location i
## more info: https://reagro.org/methods/explanatory/wofost/weather.html

#compile metadata for current site and season
trialh<-xy.df[h,"TLID"]
seasonh<-xy.df[h,"season"]
sdateh<-as.Date(as.character(xy.df[h,"startingDate"]), dformatworking)
pdateh<-as.Date(as.character(xy.df[h,"plantingDate"]), dformatworking)
hdateh<-as.Date(as.character(xy.df[h,"harvestDate"]), dformatworking)

#compile weather data for current site and season
weath<-data.frame(date=d)
weath$date<-as.Date(weath$date, dformatworking)
for (k in weathervars){
  v<-get(k)[,trialh]
  weath<-cbind(weath, v)
  names(weath)[names(weath)=="v"]<-k
}

#subset weather data to include only dates between planting and harvest
sel<-weath$date>=sdateh & weath$date<=hdateh
weath<-weath[sel,]

#convert rh to vapr (maybe good to compute air pressure from to elevation)
weath$tmean=(weath$tmin +weath$tmax)/2
weath$vapr<-1000*weath$rh*0.01*esat(TdegC=weath$tmean, Pa = 101)

#convert srad from MJ m-2 day-1 to kJ m-2 day-1
weath$srad=weath$srad*1000

#keep only relevant columns
wofostweathervars<-c("date", "srad", "tmin", "tmax", "vapr", "wind", "prec")
weath<-weath[,wofostweathervars]

#format date column
#format date variable
#weath$date<-format(weath$date, dformatmodel)

