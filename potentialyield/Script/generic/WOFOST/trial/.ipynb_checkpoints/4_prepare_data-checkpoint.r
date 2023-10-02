#read and check yield data
obs<-readRDS(yieldfile)
obs$TY<-as.numeric(obs$TY)
obs$plantingDate<-format(obs$plantingDate, dformatworking)
obs$harvestDate<-format(obs$harvestDate, dformatworking)
ymax<-aggregate(TY ~ TLID, data = obs, max)
names(ymax)[names(ymax)=="TY"]<-"ymax"

#compile meta data per trial
cols<-c("expCode","FDID","TLID","lat","lon","season","plantingDate","harvestDate")
meta<-unique(obs[, cols])

#prepare data.frame for results
xy.df<-merge(x=ymax, y=meta,  by="TLID")
sel<-complete.cases(xy.df)
xy.df<-xy.df[sel,]
xy.df$lat<-as.numeric(xy.df$lat)
xy.df$lon<-as.numeric(xy.df$lon)

# read weather data files
for(g in weathervars){
  a<-readRDS(get(paste0(g,'file')))
  #create id vector
  sel1<-5
  sel2<-sel2<-1:(ncol(a)-4)
  id<-as.character(a[sel1,sel2])
  
  #create date vector
  sel1<-6:nrow(a)
  sel2<-1
  d<-a[sel1, sel2]
  d<-as.Date(d, dformatdata)

  #create data data.frame
  sel1<-6:nrow(a)
  sel2<-2:(ncol(a)-3) 
  a<-a[sel1, sel2]
  rownames(a)<-NULL
  for (i in 1:ncol(a)){a[,i]<-as.numeric(a[,i])}
  colnames(a)<-id
  a<-cbind(d, a)
  a<-a[order(d),]
  assign(g, a)
}

d<-d[order(d)]

