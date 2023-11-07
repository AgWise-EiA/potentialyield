#prepare data
pdata<-obs
pdata$TLID<-as.factor(pdata$TLID)
pdata$N<-as.factor(10*round(pdata$N/10))
pdata$P<-as.factor(10*round(pdata$P/10))
pdata$K<-as.factor(10*round(pdata$K/10))


#create outpout directory
outdir3<-"responseplots"
wdif(file.path(outdir1, outdir2, outdir3))

#aggregate per trial and nutrient level
aggN<-aggregate(pdata$TY, list(pdata$TLID, pdata$N), max)
names(aggN)<-c("TLID", "N", "TY")
aggN$N<-as.numeric(as.character(aggN$N))
aggP<-aggregate(pdata$TY, list(pdata$TLID, pdata$P), max)
names(aggP)<-c("TLID", "P", "TY")
aggP$P<-as.numeric(as.character(aggP$P))
aggK<-aggregate(pdata$TY, list(pdata$TLID, pdata$K), max)
names(aggK)<-c("TLID", "K", "TY")
aggK$K<-as.numeric(as.character(aggK$K))

#plot
for (l in sample(length(levels(aggK$TLID)),100)){
  triall<-levels(aggK$TLID)[l]
  p<-ggplot()
  sel<-aggN$TLID==triall
  p<-p+geom_point(data=aggN[sel,], aes(x=N, y=TY, group=TLID),color="blue")
  p<-p+geom_smooth(data=aggN[sel,], aes(x=N, y=TY, group=TLID),color="blue")
  
  sel<-aggP$TLID==triall
  p<-p+geom_point(data=aggP[sel,], aes(x=P, y=TY, group=TLID),color="green")
  p<-p+geom_smooth(data=aggP[sel,], aes(x=P, y=TY, group=TLID),color="green")
  
  sel<-aggK$TLID==triall
  p<-p+geom_point(data=aggK[sel,], aes(x=K, y=TY, group=TLID),color="red")
  p<-p+geom_smooth(data=aggK[sel,], aes(x=K, y=TY, group=TLID),color="red")
  
  p<-p+scale_x_continuous(limits=c(0,100))
  #p<-p+scale_y_continuous(limits=c(0,70))
  p<-p+theme(panel.border = element_rect(colour = "black", 1, fill=NA),
             panel.grid=element_blank(),
             panel.background=element_rect(fill="white"))
  p<-p+labs(title=triall)
  p<-p+xlab("N/P/K (kg ha-1)")
  p<-p+ylab("Tuber yield (t ha-1)")
  print(p)
  
  ggsave(plot=last_plot(), 
        path=file.path(outdir1, outdir2, outdir3), 
        filename=paste0(triall, ".jpg"), 
        width=15, height=10, units="cm")
  }

