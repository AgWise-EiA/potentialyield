# install required packages (if not already installed) and load them
pkgs <- c("rstudioapi", "Rwofost", "plantecophys", "stars", "collapse","ggplot2",
          "parallel", "valmetrics", "leaflet", "htmlwidgets")
sel <- !pkgs %in% rownames(installed.packages())
if(any(sel)){install.packages(pkgs[sel])}
invisible(lapply(X=pkgs, FUN=require, character.only = TRUE))

#clean working environment
rm(list=ls())

# set working directory to folder of current script
wd<-dirname(getSourceEditorContext()$path)
setwd(wd)

# make settings
source("1_make_settings.r")

# define functions
source("2_define_functions.r")

# create the output directories if they do not already exist
wdif(outdir1); wdif(file.path(outdir1, outdir2))

# import and prepare data
source("3_prepare_data.r")

# create function to prepare data and run model for each trial
f<-function(h){
  ## prepare data and parameters
  source("4b_prepare_list_weather.r", local=TRUE)
  source("4a_prepare_list_crop.r", local=TRUE)
  source("4c_prepare_list_soil.r", local=TRUE)
  source("4d_prepare_list_control.r", local=TRUE)
  ## run model
  source("5_run_model.r", local=TRUE)
  return(wso)
}

#do a test to assess time it will take
t0<-Sys.time()
cores<- max(detectCores()-5, 1) #minus ten , not to overload server
m<-matrix(20:120)
a<-dapply(X=m, FUN=f, MARGIN = 1, parallel = TRUE, mc.cores = cores,
       return = "matrix", drop = TRUE)
t1<-Sys.time()
tdiff<-as.numeric((t1-t0))
ttot<-round(tdiff*nrow(xy.df)*0.01/60,2)
print(paste0("this will take ",ttot," minutes (",round(ttot/60,2)," hours)"))

#run it
cores<- max(detectCores()-10, 1) #minus ten , not to overload server
m<-matrix(1:nrow(xy.df))

xy.df$wso<-dapply(X=m, FUN=f, MARGIN = 1, parallel = TRUE, mc.cores = cores,
       return = "matrix", drop = TRUE)

# map results
source("6_map_results.r")

# plot results
source("7_plot_results.r")


