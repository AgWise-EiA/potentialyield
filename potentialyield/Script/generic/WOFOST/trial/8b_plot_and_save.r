#prepare
E<-round(e(o=dd$ymax, p=dd$wso),2)
R2<-round(r2(o=dd$ymax, p=dd$wso),2)
MAE<-round(mae(o=dd$ymax, p=dd$wso),1)
ts<-paste0("E = ", E, ", r2 = ", R2,  ", MAE = ", MAE)
tx<-expression("Maxumum observed yield, tonnes freshweight ha"^-1)
ty<-expression("Predicted yield, tonnes freshweight ha"^-1)
rg<-range(brks)
diag<-data.frame(x=rg, y=rg)

#plot
p<-ggplot(data=dd, aes(x=ymax, y=wso))
p<-p+geom_point(aes(color=adm2, shape =season), size=4, stroke=1)
p<-p+geom_smooth(method ="lm", se=F, linewidth=0.5, color="black", linetype="longdash")
p<-p+geom_line(data=diag, aes(x,y), linewidth=1, color="grey")
p<-p+scale_y_continuous(breaks=brks, limits=rg, name=ty)
p<-p+scale_x_continuous(breaks=brks, limits=rg, name=tx)
p<-p+scale_shape_discrete(solid=F, name = "season")
p<-p+scale_color_discrete(name=admtype)
p<-p+coord_equal()
p<-p+theme(panel.border = element_rect(colour = "black", 1, fill=NA),
           panel.grid=element_blank(),
           panel.background=element_rect(fill="white"))
p<-p+labs(title=tm, subtitle=ts)
print(p)

#save plot
ggsave(plot=last_plot(), 
       path=file.path(outdir1, outdir2), filename=fname, 
       width=15, height=15, units="cm")

