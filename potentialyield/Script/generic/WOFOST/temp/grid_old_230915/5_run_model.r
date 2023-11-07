#create model object and run it
mod<-wofost_model(crop, weath, soil, contr)
suppressWarnings(y<-run(mod))

#extract the built-up crop yield
wso<-round(max(y$WSO, na.rm=TRUE)/1000)
xy.df[h,"wso"]<-wso

#plot
#plot(y$date, y$LAI, cex=.5)
#plot(y$date, y$WSO, cex=.5)