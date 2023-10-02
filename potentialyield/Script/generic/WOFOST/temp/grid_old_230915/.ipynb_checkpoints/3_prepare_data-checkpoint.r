# read weather data files
for(g in weathervars){
  a<-readRDS(get(paste0(g,'file')))
  #create date vector
  sel1<-5:nrow(a)
  d<-rownames(a)[sel1]
  d<-as.Date(d, dformatdata)
  
  #create data data.frame
  sel2<-1:(ncol(a)-4) 
  a<-a[sel1, sel2]
  rownames(a)<-NULL
  for (i in 1:ncol(a)){a[,i]<-as.numeric(a[,i])}
  a<-cbind(d, a)
  a<-a[order(d),]
  assign(g, a)
} 
d<-d[order(d)]
  
#create a data.frame for registration of results
a<-readRDS(get(paste0(g,'file')))
lon<-as.numeric(a[1, sel2])
lat<-as.numeric(a[2, sel2])
id<-colnames(a[, sel2])
xy.df<-data.frame(id, lon, lat)
xy.df$season<-"2022B"
