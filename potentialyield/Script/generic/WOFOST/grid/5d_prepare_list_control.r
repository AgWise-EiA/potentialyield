#create a list of control parameters
##info: https://reagro.org/methods/explanatory/wofost/control.html

#get predefined parameters
contr <- wofost_control()

#prepare dates (p= planting, h=harvest, s=model start, m= month, d = day of month)
pm<-phdates$pmonth[p] 
pd<-phdates$pday[p] 
pdate<-as.Date(ISOdate(yearh,pm, pd))

hm<-phdates$hmonth[p]
hd<-phdates$hday[p]
hdate<-as.Date(ISOdate(yearh,hm, hd))

sm<-pm-1
sd<-pd
sdate<-as.Date(ISOdate(yearh,sm, sd))

#set run parameters
contr$modelstart<-sdate
contr$cropstart<-as.numeric(pdate-sdate) 
contr$start_sowing<-1 #start crop at sowing
contr$maxduration<-as.numeric(hdate-pdate)
contr$water_limited<-1
contr$waterlim_type<-0

#set site parameters
contr$latitude<-xy.df[h,"lat"]
contr$elevation<-soildata[h,"altitude"]
contr$co2<-422 #https://climate.nasa.gov/vital-signs/carbon-dioxide/ checked 2023-08-26
