#create a list of control parameters
##info: https://reagro.org/methods/explanatory/wofost/control.html

#get predefined parameters
contr <- wofost_control()

#set run parameters
contr$modelstart<-sdateh
contr$cropstart<-as.numeric(pdateh-sdateh) 
contr$start_sowing<-1 #start crop at sowing
contr$maxduration<-as.numeric(hdateh-pdateh)
contr$water_limited<-1
contr$waterlim_type<-0

#set site parameters
contr$latitude<-xy.df[h,"lat"]
contr$elevation<-soildata[h,"altitude"]
contr$co2<-422 #https://climate.nasa.gov/vital-signs/carbon-dioxide/ checked 2023-08-26
