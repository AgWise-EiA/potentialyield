#create model object and run it
mod<-wofost_model(crop, weath, soil, contr)
suppressWarnings(y<-run(mod))

#extract the built-up crop yield
##denominator 1000 to convert to tonnes per ha 
##denominator 0.2 to convert to fresh weight (typical potato water content is 20%)
dmyield<-round(max(y$WSO, na.rm=TRUE)/1000/0.2,3)

#create yield vector if it does not exist
if(!exists("dmyieldfour")) dmyieldfour<-as.numeric(vector())

#add wso to yield vector
dmyieldfour<-c(dmyieldfour, dmyield)

#compute total precipitation from model start (1 month before planting)
sel<-weath$date>=pdate & weath$date<=hdate
totprec<-round(sum(weath[sel, "prec"]))

#create precipitation vector if it does not exist
if(!exists("totprecfour")) totprecfour<-as.numeric(vector())

#add totprec to precipitation vector
totprecfour<-c(totprecfour, totprec)

#plot
#plot(y$date, y$LAI, cex=.5)
#plot(y$date, y$WSO, cex=.5)
#plot(y$date, y$SM, cex=.5)
