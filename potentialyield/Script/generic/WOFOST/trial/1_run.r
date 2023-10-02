# install required packages (if not already installed) and load them
pkgs <- c("terra", "rstudioapi", "Rwofost", "plantecophys", "stars", "collapse",
          "ggplot2","parallel", "valmetrics", "leaflet", "htmlwidgets", "FNN",
          "geodata")
sel <- !pkgs %in% rownames(installed.packages())
if(any(sel)){install.packages(pkgs[sel])}
invisible(lapply(X=pkgs, FUN=require, character.only = TRUE))

#clean working environment
rm(list=ls())

# set working directory to folder of current script
wd<-dirname(getSourceEditorContext()$path)
setwd(wd)

# make settings
source("2_make_settings.r")

# define functions
source("3_define_functions.r")

# create the output directories if they do not already exist
wdif(outdir1); wdif(file.path(outdir1, outdir2))# import and prepare data
source("4_prepare_data.r")

# create function to prepare data and run model for one trial
f<-function(h){
  ## prepare data and parameters
  source("5a_prepare_list_weather.r", local=TRUE)
  source("5b_prepare_list_crop.r", local=TRUE)
  source("5c_prepare_list_soil.r", local=TRUE)
  source("5d_prepare_list_control.r", local=TRUE)
  ## run model
  source("6_run_model.r", local=TRUE)
  return(wso)
}

#run  function to prepare data and run model for each trial (each row in xy.df)
cores<- max(detectCores()-3, 1) #minus 3, not to overload server
m<-matrix(1:nrow(xy.df))
xy.df$wso<-dapply(X=m, FUN=f, MARGIN = 1, parallel = TRUE, mc.cores = cores,
       return = "matrix", drop = TRUE)

# map results
if(map_results) source("7_map_results.r")

# plot results
source("8_plot_results.r") #this in turn sources script 8b

# 9 export results
fname<-file.path(outdir1, outdir2, "trial.txt")
write.table(xy.df, fname, row.names = F, sep = "t")


