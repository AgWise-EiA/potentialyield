#read, project and subset boundary data
##aez
aoi<-vect(aoifile)
aoi<-project(aoi, "EPSG: 4326")
sel<-aoi$Names_AEZs %in% c("Birunga", "Buberuka highlands", "Congo-Nile watershed divide")
aoi<-aoi[sel,]
names(aoi)<-c("aez", "id")
print("aez polygons imported and prepared")

##administrative boundaries
adm <- gadm(country=country, level=2, path=tempdir())
adm$adm2<-adm$"NAME_2"
admtype<-unique(adm$"TYPE_2")
print("administrative boundaries imported and prepared")

# read first weather data files for metadata
g<-weathervars[1]
a<-readRDS(get(paste0(g,'file')))
  
##create a data.frame with metadata for logging of results
sel<-names(a)%in%weathermetavars
xy.df<-a[,weathermetavars]

##fix column names
xy.df$planingDate<-xy.df$startingDate #remember to remove
names(xy.df)[names(xy.df)==longitudevar]<-"lon" 
names(xy.df)[names(xy.df)==latitudevar]<-"lat"
names(xy.df)[names(xy.df)==idvar]<-"ID"
  
#fix formats
xy.df$lat<-as.numeric(xy.df$lat)
xy.df$lon<-as.numeric(xy.df$lon)

#add year column and create a new id column whish contains both ID, 
# which is unique per location and year
xy.df$year<-format(as.Date(xy.df$startingDate), "%Y")
xy.df$siteyear<-paste0("site", xy.df$ID, "year",xy.df$year)

#extract aez name for each trial, and exclude grid points outside aez
xy.sp<-vect(x=xy.df, geom=c("lon", "lat"), crs=crs(aoi))
xy.sp$aez<-extract(aoi, xy.sp)$aez
aez<-xy.sp$aez
xy.df<-cbind(xy.df, aez)
names(xy.df)[ncol(xy.df)]<-"aez"
sel<-!is.na(xy.df$aez)
xy.df<-xy.df[sel,]

#extract administrative region name for each trial
xy.sp<-vect(x=xy.df, geom=c("lon", "lat"), crs=crs(adm))
xy.sp$adm2<-extract(adm, xy.sp)$adm2
adm2<-xy.sp$adm2
xy.df<-cbind(xy.df, adm2)
names(xy.df)[ncol(xy.df)]<-"adm2"

#read all weather data files (for date and weather data)
for(g in weathervars){
  #import  
  a<-readRDS(get(paste0(g,'file')))
  
  #select only gridpints in AEZ
  sel<-a$ID %in% xy.df$ID
  a<-a[sel,]
  
  #create date vector (d) and a weather data data.frame (a)
  sel<-grepl(x=colnames(a), pattern="-") #to identify the column names that are dates
  a<-a[,sel]
  d<-colnames(a)
  d<-unlist(strsplit(d,"_"))
  sel<-grepl(x=d, pattern="-")#to identify the items that are dates 
  d<-d[sel]
  d<-as.Date(d, dformatdata)
  rownames(a)<-xy.df$siteyear
  colnames(a)<-NULL
  a<-t(a)
  
  #order and assign
  a<-a[order(d),]
  assign(g, a)
  print(paste0(g, " imported and prepared"))
}

#order date vector
d<-d[order(d)]

#read soil data files
soildata<-readRDS(soilfile)
names(soildata)[names(soildata)==idvar]<-"ID"
print("soil data imported and prepared")





