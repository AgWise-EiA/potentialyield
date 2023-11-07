#found this grid on the server, what grid is it?
#GPSdata_AOI <- readRDS("~/agwise/AgWise_Data/data_sourcing/UseCase_Rwanda_RAB/Potato/raw/AOI_GPS.RDS")
#plot(GPSdata_AOI$longitude, GPSdata_AOI$latitude)

#create raster
xy<-expand.grid(unique(xyz$x),unique(xyz$y))
r<-terra::rast(xy, type="xyz")
r<-extend(r,1)
crs(r)  <- "epsg:4326"

for (i in seasons) {
  sel<-xy.df$year==i
  xyz<-xy.df[sel,c("x","y", "wso")]

  #create vector of points for predictions
  pts <- vect(xyz, geom=c('x','y'), crs="epsg:4326")

  #interpolate to grid
  rd<-0.5*sqrt(expanse(r)$area)
  r$a<-interpNear(x=r, y=pts, radius=rd, field="wso", interpolate=TRUE, fill=NA)
  plot(r$a, main=i)
  names(r)[names(r)=="a"]<-paste0("year", i)
}

#mask by administrative boundary
aoi<-gadm(country="rwanda", level=0, path=tempdir())
r<-mask(r, aoi)

#make mapping window
e<-vect(1.2*ext(r), crs(r))
w<-e-aoi

#scale unit
r<-0.001*r

#prepare for plotting
brks<-pretty(values(r),5)

#plot

for (i in (1:dim(r)[3])){
  g<-ggplot()
  g<-g+geom_spatraster(data=r[[i]])
  g<-g+labs(title= names(r[[i]]))
  g<-g+geom_spatvector(data=w, fill="white")
  g<-g+scale_fill_cross_blended_tint_b("arid", 
                                     values=brks, n.breaks=length(brks), direction =-1,
                                   name='Yield')
  print(g)
}

fname<-file.path(outdir1, outdir2, "map.jpg")
ggsave(
  filename =fname,
  plot = last_plot(),
  device = "jpeg",
  path = NULL,
  scale = 1,
  width = 200,
  height = 100,
  units = "mm",
  dpi = 300,
)

