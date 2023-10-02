# install required packages (if not already installed) and load them
pkgs <- c("terra", "rstudioapi", "Rwofost", "plantecophys", "stars", "collapse",
          "ggplot2","parallel", "valmetrics", "leaflet", "htmlwidgets", "FNN",
          "geodata", "plyr", "sf")
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
  ## run model for the four planting dates
  for(p in 1:4){
    source("5d_prepare_list_control.r", local=TRUE)
    source("6_run_model.r", local=TRUE)
  }
  fh<-c(dmyieldfour, totprecfour)
  print(fh)
  return(fh) #this is a vector of predicted dry matter yield for the four dates, followed by the four  precipitation totals from model start till harvest
}

#run  function to prepare data and run model for each trial (each row in xy.df)
cores<- max(detectCores()-10, 1) #minus x, not to overload server
n<-nrow(xy.df)
m<-matrix(1:n)
cols<-c(paste0("yield", 1:4), paste0("prec", 1:4))
xy.df[1:n,cols]<-dapply(X=m, FUN=f, MARGIN = 1, parallel = TRUE, mc.cores = cores,
       return = "matrix", drop = TRUE)

# 9 export results
cols<-c("lon", "lat", "ID", "aez", "NAME_1" , "NAME_2","year","siteyear",
        "yield1","yield2","yield3","yield4",
        "prec1", "prec2", "prec3", "prec4" )
fname<-file.path(outdir1, outdir2, "grid.txt")
write.table(xy.df[,cols], fname, row.names = F, sep = "t")


