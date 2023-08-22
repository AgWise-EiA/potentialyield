#create a list of control parameters
contr <- wofost_control()

#modify it for current run
contr$modelstart<-min(weath$date)
contr$maxduration<-length(weath$date)
#contr$latitude
#contr$elevation

