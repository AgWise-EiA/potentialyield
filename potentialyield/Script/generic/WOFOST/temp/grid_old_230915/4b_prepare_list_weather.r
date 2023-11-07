#create the data.frame with weather data for location i
##date # shall have this format: 1974-05-09 and a daily time step
##srad	Solar radiation	(kJ m-2 day-1)
##tmin	Minimum temperature	(degrees C)
##tmax	Maximum temperature	(degrees C)
##vapr	Vapor pressure (kPa)
##wind	Wind speed	(m s-1)
##prec	Precipitation	(mm day-1)

cellh<-xy.df[h,"id"]
seasonh<-xy.df[h,"season"]
#pdateh<-as.Date(xy.df[h,"plantingDate"], dformatworking)
#hdateh<-as.Date(xy.df[h,"harvestDate"], dformatworking)
pdateh<-min(tmin$d[tmin$d>=as.Date("2022-01-01", dformatworking)])
hdateh<-max(tmin$d[tmin$d>=as.Date("2022-01-01", dformatworking)])
sel<-weath$date>=pdateh & weath$date<=hdateh

#compile all weather variables for trial h
weath<-srad$d
sel<-weath$date>=pdateh & weath$date<=hdateh
weath<-weath[sel,]
for (k in weathervars){
  v<-get(k)
  sel<-v$date>=pdateh & v$date<=hdateh
  v<-v[sel,cellh]
  weath<-cbind(weath, v)
  names(weath)[names(weath)=="v"]<-k
}
sel<-weath$date>=pdateh & weath$date<=hdateh
weath<-weath[sel,]

#convert rh to vapr
weath$tmean=(weath$tmin +weath$tmax)/2
weath$vapr<-weath$rh*0.01*esat(TdegC=weath$tmean, Pa = 101)

