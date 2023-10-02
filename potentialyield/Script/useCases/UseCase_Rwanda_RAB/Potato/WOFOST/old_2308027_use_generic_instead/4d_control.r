#create a list of control parameters
contr <- wofost_control()

#modify it for current run
contr$modelstart<-as.Date(pdateh, dformatmodel)
contr$maxduration<-as.numeric(hdateh-pdateh)
contr$latitude<-xy.df[h,"lat"]
#contr$elevation
contr$co2<-422 #https://climate.nasa.gov/vital-signs/carbon-dioxide/ checked 2023-08-26