#view first ten rows and first ten cols of data
a[1:10, 1:10]

#view first ten rows and first ten cols of data
a[1:10, ncol(a)-(10:1)]


sel<-as.logical(a["ID",]==trialh)
sel[is.na(sel)]<-FALSE
a[,sel]

nrow(weathermeta)
nrow(xy.df)




#expand xy.df with seasons
seasons<-c("A")
a<-expand.grid(xy.df$ID, years, seasons)
names(a)<-c("ID", "year", "season1")
a$season2<-paste0(a$year, a$season1)
row.names(a)<-NULL
row.names(xy.df)<-NULL
nearest<-get.knnx(data=xy.df[, "ID"], query=a[, "ID"], k=1)$nn.index
xy.df<-cbind(a, xy.df[nearest, ])



for (ii in 1:nrow(weathermeta)){
  sel<-{
    weathermeta[ii, "lat"]==xy.df[,"lat"]& 
      #weathermeta[ii, "lon"]==xy.df[,"lon"] & #one of the longitudes is strange. Do not understand the problem
      weathermeta[ii, "harvestDate"]==xy.df[,"harvestDate"]}
  weathermeta[ii, "TLID"]<-xy.df[sel,"TLID"]
  weathermeta[ii, "plantingDate"]<-xy.df[sel,"plantingDate"]
  weathermeta[ii, "harvestDate"]<-xy.df[sel,"harvestDate"]
  weathermeta[ii, "lon"]<-xy.df[sel,"lon"] #fix longitude problem
  
  #add Startingdate
