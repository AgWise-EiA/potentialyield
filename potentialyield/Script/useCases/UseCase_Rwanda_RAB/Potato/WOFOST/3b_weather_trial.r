#create the data.frame with weather data for location i
##date # shall have this format: 1974-05-09 and a daily time step
##srad	Solar radiation	(kJ m-2 day-1)
##tmin	Minimum temperature	(degrees C)
##tmax	Maximum temperature	(degrees C)
##vapr	Vapor pressure (kPa)
##wind	Wind speed	(m s-1)
##prec	Precipitation	(mm day-1)

trialh<-xy.df[h,"ID"]
seasonh<-xy.df[h,"season"]
pdateh<-xy.df[h,"plantingDate"]
hdateh<-xy.df[h,"harvestDate"]

#compile all weath variables for location i and season j
sel1<-d>=pdateh&d<=hdateh
sel2<-as.numeric(tmin[rownames(tmin)=="ID",]==trialh)
weath<-data.frame(date=d[sel1])
for (k in weathervars){
  v<-as.numeric(get(k)[sel1,sel2])
  weath<-data.frame(weath,v)
  names(weath)[names(weath)=="v"]<-k
}

#convert rh to vapr
weath$tmean=(weath$tmin +weath$tmax)/2
weath$vapr<-weath$rh*0.01*esat(TdegC=weath$tmean, Pa = 101)

