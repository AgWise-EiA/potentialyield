#prepare
xy.df$season<-as.factor(xy.df$season)
xy.df$residual<-xy.df$wso-xy.df$ymax

#aggregate per season and aez
aggymax<-aggregate(xy.df$ymax, list(xy.df$season, xy.df$aez), max)
aggwso<-aggregate(xy.df$wso, list(xy.df$season, xy.df$aez), mean)
agg<-cbind(aggymax, aggwso$x)
names(agg)<-c("season", "aez", "ymax", "wso")

#### plot at aggregated level####
E<-round(e(o=xy.df$ymax, p=xy.df$wso),2)
R2<-round(r2(o=xy.df$ymax, p=xy.df$wso),2)

brks<-pretty(c(xy.df$ymax, xy.df$wso))
rg<-range(brks)
diag<-data.frame(x=rg, y=rg)

#plot 
p<-ggplot(data=xy.df, aes(x=ymax, y=wso))
p<-p+geom_point(aes(color=season, shape =aez), size=4, stroke=1)
p<-p+geom_line(data=diag, aes(x,y), linewidth=1, color="grey")
p<-p+scale_y_continuous(breaks=brks, limits=rg)
p<-p+scale_x_continuous(breaks=brks, limits=rg)
p<-p+scale_shape_discrete(solid=F)
p<-p+coord_equal()
p<-p+theme(panel.border = element_rect(colour = "black", 1, fill=NA),
           panel.grid=element_blank(),
           panel.background=element_rect(fill="white"))
p<-p+labs(title=paste0("cultivar = ", species, " soil = ", ss),
          subtitle=paste0("E = ", E, ", r2 = ", R2))
print(p)

ggsave(plot=last_plot(), 
       path=file.path(outdir1, outdir2), filename="scatter_trial.jpg", 
       width=15, height=10, units="cm")



#####plot at aggregated level####
E<-round(e(o=agg$ymax, p=agg$wso),2)
R2<-round(r2(o=agg$ymax, p=agg$wso),2)

brks<-pretty(c(agg$ymax, agg$wso))
rg<-range(brks)
diag<-data.frame(x=rg, y=rg)

#plot 
p<-ggplot(data=agg, aes(x=ymax, y=wso))
p<-p+geom_point(aes(color=season, shape =aez), size=4, stroke=1)
p<-p+geom_line(data=diag, aes(x,y), size=1, color="grey")
p<-p+scale_y_continuous(breaks=brks, limits=rg)
p<-p+scale_x_continuous(breaks=brks, limits=rg)
p<-p+scale_shape_discrete(solid=F)
p<-p+coord_equal()
p<-p+theme(panel.border = element_rect(colour = "black", 1, fill=NA),
           panel.grid=element_blank(),
           panel.background=element_rect(fill="white"))
p<-p+labs(title=paste0("cultivar = ", species, " soil = ", ss),
          subtitle=paste0("E = ", E, ", r2 = ", R2))
print(p)

ggsave(plot=last_plot(), 
       path=file.path(outdir1, outdir2), filename="scatter_agg.jpg", 
       width=15, height=10, units="cm")

