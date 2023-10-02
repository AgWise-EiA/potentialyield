xy<-c('longitude', 'latitude')

x<-rep(tmax['longitude',locations], length(seasons))
y<-rep(tmax['latitude',locations], length(seasons))
year<-rep(seasons, length(locations))
xy.df<-cbind(x,y,year)


lx<-length(x)
ly<-length(y)
lz<-length(year)
aa<-array(rep(NA, lx*ly*lz), dim=c(lx, ly, lz))

x<-as.numeric(unique(xy.df$longitude))
y<-as.numeric(unique(xy.df$latitude))
year<-as.numeric(min(seasons):max(seasons))
xyz<-expand.grid(x, y, year)
xyz<-data.frame(x=xyz[,1], y=xyz[,2], year=xyz[,3]) #workaround to solve problem with structure 


#i<- locations[300] #just while testing
#j<- seasons[18] #just while testing


# Print progress
cat("Finished", h, "of", nrow(xy.df), "\n")

# setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(max(cores[1]-10, 1)) #not to overload server
registerDoParallel(cl)

# prepare data and run model for each location and season
a<-foreach(h=20:55, .combine=c, .packages=Rwofost) %dopar% {
  ## prepare data and parameters
  source(file.path("r","3a_crop.r"))
  source(file.path("r","3b_weather.r"))
  source(file.path("r","3c_soil.r"))
  source(file.path("r","3d_control.r"))
  ## run model
  source(file.path("r","4_model.r"))
  ## Print progress
  cat("Finished", h, "of", nrow(xy.df), "\n")
  return(wso)
}
x.df$wso<-a

# stop cluster
stopCluster(cl)