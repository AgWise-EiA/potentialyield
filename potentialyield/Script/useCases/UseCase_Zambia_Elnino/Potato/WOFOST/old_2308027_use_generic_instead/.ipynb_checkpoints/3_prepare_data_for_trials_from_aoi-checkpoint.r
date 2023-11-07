#read and check yield data
obs<-readRDS(yieldfile)
obs$TY<-as.numeric(obs$TY)
obs$plantingDate<-format(obs$plantingDate, dateformat)
obs$harvestDate<-format(obs$harvestDate, dateformat)
ymax<-aggregate(TY ~ TLID, data = obs, max)
names(ymax)[names(ymax)=="TY"]<-"ymax"

#compile meta data per trial
cols<-c("expCode","FDID","TLID","lat","lon","season","plantingDate","harvestDate")
meta<-unique(obs[, cols])

#prepare data.frame for results
xy.df<-merge(x=ymax, y=meta,  by="TLID")
sel<-complete.cases(xy.df)

# read weather data files
for(g in weathervars){
  a<-readRDS(get(paste0(g,'file')))
  #subset period under which the trials were conducted
  aa<-c(rep("05_02_1979", 4), row.names(a)[5:nrow(a)])
  aa<-as.Date(aa, dateformat)
  sel<-aa>=min(obs$plantingDate, na.rm=T) & aa<=max(obs$harvestDate, na.rm=T)
  a<-a[sel,]
  #create id vector
  sel1<-5
  sel2<-sel2<-1:(ncol(a)-4)
  id<-as.character(a[sel1,sel2])
  
  #create date vector
  sel1<-6:nrow(a)
  sel2<-1
  d<-a[sel1, sel2]
  d<-as.Date(d, dateformat)
  
  #create data data.frame
  sel1<-6:nrow(a)
  sel2<-2:(ncol(a)-3) 
  a<-a[sel1, sel2]
  rownames(a)<-NULL
  a<-as.data.frame(apply(X=a, MARGIN=2, FUN=as.numeric))
  colnames(a)<-id
  a<-cbind(d, a)
  a<-a[order(d),]
  assign(g, a)
}

