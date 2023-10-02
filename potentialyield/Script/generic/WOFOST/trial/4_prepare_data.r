#read project and subset boundary data
##aez
aoi<-vect(aoifile)
aoi<-project(aoi, "EPSG: 4326")
sel<-aoi$Names_AEZs %in% c("Birunga", "Buberuka highlands", "Congo-Nile watershed divide")
aoi<-aoi[sel,]
names(aoi)<-c("aez", "id")

##adminisrative boundaries
adm <- gadm(country="rwanda", level=2, path=tempdir())
adm$adm2<-adm$"NAME_2"
admtype<-unique(adm$"TYPE_2")

#read and check yield data
obs<-readRDS(yieldfile)
obs$TY<-as.numeric(obs$TY)
obs$plantingDate<-format(obs$plantingDate, dformatworking)
obs$harvestDate<-format(obs$harvestDate, dformatworking)
ymax<-aggregate(TY ~ TLID, data = obs, max)
names(ymax)[names(ymax)=="TY"]<-"ymax"
ymax$ymax<-ymax$ymax/0.8 #conversion factor to compensate for sub-optimal management, fertilization, genotyope etc. 

#compile meta data per trial
cols<-c("expCode","FDID","TLID","lat","lon","season","plantingDate","harvestDate")
meta<-unique(obs[, cols])

#prepare data.frame for results
xy.df<-merge(x=ymax, y=meta,  by="TLID")
sel<-complete.cases(xy.df)
xy.df<-xy.df[sel,]
xy.df$lat<-as.numeric(xy.df$lat)
xy.df$lon<-as.numeric(xy.df$lon)
xy.df$harvestDate<-as.Date(as.character(xy.df$harvestDate), dformatworking)
xy.df$plantingDate<-as.Date(as.character(xy.df$plantingDate), dformatworking)

#extract aez name for each trial
xy.sp<-vect(x=xy.df, geom=c("lon", "lat"), crs=crs(aoi))
xy.sp$aez<-extract(aoi, xy.sp)$aez
aez<-xy.sp$aez
xy.df<-cbind(xy.df, aez)
names(xy.df)[ncol(xy.df)]<-"aez"

#extract administrative region name for each trial
xy.sp<-vect(x=xy.df, geom=c("lon", "lat"), crs=crs(adm))
xy.sp$adm2<-extract(adm, xy.sp)$adm2
adm2<-xy.sp$adm2
xy.df<-cbind(xy.df, adm2)
names(xy.df)[ncol(xy.df)]<-"adm2"

# read first weather data files (for metadata)
g<-weathervars[1]
a<-readRDS(get(paste0(g,'file')))
  
#create metadata data.frame
sel<-names(a)%in%weathermetavars
weathermeta<-a[,weathermetavars]

##make naming consistent with the yield data
names(weathermeta)[names(weathermeta)==longitudevar]<-"lon" 
names(weathermeta)[names(weathermeta)==latitudevar]<-"lat"
names(weathermeta)[names(weathermeta)==hdatevar]<-"harvestDate"
  
##fix data format
weathermeta$lat<-as.numeric(weathermeta$lat)
weathermeta$lon<-as.numeric(weathermeta$lon)
weathermeta$harvestDate<-as.Date(weathermeta$harvestDate)
  
#add TLID to weathermeta (workaround because on unsorted problems with merge)
weathermeta$temp<-as.numeric(weathermeta$harvestDate)
xy.df$temp<-as.numeric(xy.df[, "harvestDate"])
mergevars<-c("lon", "lat", "temp")
nearest<-get.knnx(data=xy.df[, mergevars],
                    query=weathermeta[, mergevars],
                    k=1)$nn.index
TLID<-xy.df[nearest, "TLID"]
weathermeta<-cbind(weathermeta, TLID)

#add startingDate to xy.df (workaround because on unsorted problems with merge)
nearest<-get.knnx(data=weathermeta[, mergevars],
                    query=xy.df[, mergevars],
                    k=1)$nn.index
startingDate<-weathermeta[nearest, "startingDate"]
xy.df<-cbind(xy.df, startingDate)
xy.df$startingDate<-as.Date(xy.df$startingDate)

#read all weather data files (for date and weather data)
for(g in weathervars){
    a<-readRDS(get(paste0(g,'file')))
  
  #create date vector (d) and a weather data data.frame (a)
  sel<-!colnames(a)%in%weathermetavars
  a<-a[,sel]
  d<-colnames(a)
  d<-unlist(strsplit(d,"_"))
  sel<-is.even(1:length(d))
  d<-d[sel]
  d<-as.Date(d, dformatdata)
  rownames(a)<-weathermeta$TLID
  colnames(a)<-NULL
  a<-t(a)
  
  #order and assign
  a<-a[order(d),]
  assign(g, a)
}

#order date vector
d<-d[order(d)]

#drop temproary columns
xy.df$temp<-NULL
xy.df$weaterhmeta<-NULL

#read soil data files
a<-readRDS(soilfile)
soildata<-xy.df[, c("lon", "lat", "TLID")]

#add trial id
nearest<-get.knnx(data=xy.df[, c("lon", "lat")], 
                  query=soildata[, c("lon", "lat")],
                  k=1)$nn.index
soildata<-cbind(soildata, a[nearest,])





