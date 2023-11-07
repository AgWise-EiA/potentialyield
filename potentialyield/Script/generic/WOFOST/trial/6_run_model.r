#create model object and run it
mod<-wofost_model(crop, weath, soil, contr)
suppressWarnings(y<-run(mod))

#extract the built-up crop yield
##denominator 1000 to convert to tonnes per ha 
##denominator 0.2 to convert to fresh weight (typical potato water content is 20%)
wso<-round(max(y$WSO, na.rm=TRUE)/1000/0.2)
xy.df[h,"wso"]<-wso

#plot
plot(y$date, y$LAI, cex=.5)
plot(y$date, y$WSO, cex=.5)
