mod<-wofost_model(crop, weath, soil, contr)
y<-run(mod)
wso<-round(max(y$WSO, na.rm=TRUE))
