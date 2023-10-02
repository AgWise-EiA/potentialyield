#prepare data
xy.df$season<-as.factor(xy.df$season)
xy.df$residual<-xy.df$wso-xy.df$ymax

#aggregate per season and adm2
aggymax<-aggregate(xy.df$ymax, list(xy.df$season, xy.df$adm2), max)
aggwso<-aggregate(xy.df$wso, list(xy.df$season, xy.df$adm2), mean)
agg<-cbind(aggymax, aggwso$x)
names(agg)<-c("season", "adm2", "ymax", "wso")

#prepare plotting at trial level
dd<-xy.df
tm<-"Per trial"
brks<-pretty(c(dd$ymax, dd$wso))
fname<-"scatter_trial.jpg"

#plot and save at trial level
source("8b_plot_and_save.r")

#prepare plotting at aggregated level
dd=agg
tm<-"Per adminstrative unit and season"
#brks<-pretty(c(agg$ymax, agg$wso)) #use same breaks as for trial level
fname<-"scatter_agg.jpg"

#plot and save at aggregated level
source("8b_plot_and_save.r")

