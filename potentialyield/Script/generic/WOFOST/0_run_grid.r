# install required packages from  (if not already installed) and load them
pkgs <- c("rstudioapi", "Rwofost", "plantecophys", "stars", "collapse","ggplot2", "parallel", "terra", "remotes", "tidyterra")
sel <- !pkgs %in% rownames(installed.packages())
if(any(sel)){install.packages(pkgs[sel])}
if(!"geodata" %in% rownames(installed.packages())) remotes::install_github("rspatial/geodata")
pkgs<-c(pkgs, "geodata")
invisible(lapply(X=pkgs, FUN=require, character.only = TRUE))

# set working directory to folder of current script
wd<-dirname(getSourceEditorContext()$path)
setwd(wd) 

# make settings
source("1_settings_grid.r")

# define functions
source(file.path(scrdir,"2_functions.r"))

# create the output directories if they do not already exist
wdif(outdir1)
wdif(file.path(outdir1, outdir2))

# read weather data files
for(g in weathervars){assign(g, readRDS(get(paste0(g,'file'))))}

#prepare data.frame for results
xy.df<-data.frame(location=rep(locations, length(seasons)))
xy.df$x<-as.numeric(rep(tmax['longitude',locations], length(seasons)))
xy.df$y<-as.numeric(rep(tmax['latitude',locations], length(seasons)))
xy.df$year<-rep(seasons, length(locations))

# create function to prepare data and run model for each location and season
f<-function(h){
  ## prepare data and parameters
  source(file.path(scrdir,"3a_crop.r"), local=TRUE)
  source(file.path(scrdir,"3b_weather.r"), local=TRUE)
  source(file.path(scrdir,"3c_soil.r"), local=TRUE)
  source(file.path(scrdir,"3d_control.r"), local=TRUE)
  ## run model
  source(file.path(scrdir,"4_model.r"), local=TRUE)
  return(wso)
}

#do a test to assess time it will take
cores<- max(detectCores()-5, 1) #minus five , not to overload server
timetest(cores=cores)

#run it
m<-matrix(1:nrow(xy.df))
xy.df$wso<-dapply(X=m, FUN=f, MARGIN = 1, parallel = TRUE, mc.cores = cores,
       return = "matrix", drop = TRUE)

#save Rdata.file
save(xy.df, file="a.RData")

# compile results
source(file.path(scrdir,"5_results.r"))

# map results
#source(file.path(scrdir,"6_maps.r"))

# plot results
#source(file.path(scrdir,"7_plots.r"))
