# install required packages (if not already installed) and load them
pkgs <- c("rstudioapi", "Rwofost", "plantecophys", "stars", "collapse","ggplot2", "parallel")
sel <- !pkgs %in% rownames(installed.packages())
if(any(sel)){install.packages(pkgs[sel])}
invisible(lapply(X=pkgs, FUN=require, character.only = TRUE))

#clean working environment
rm(list=ls())

# set working directory to folder of current script
wd<-dirname(getSourceEditorContext()$path)
setwd(wd) 

# make settings
source("1_settings_trials.r")

# define functions
source(file.path(scrdir,"2_functions.r"))

# create the output directories if they do not already exist
wdif(outdir1); wdif(file.path(outdir1, outdir2))

# import and prepare data
source(file.path(scrdir,"3_prepare_data.r"))

# create function to prepare data and run model for each trial
for (h in 1:nrow(xy.df)){
  ## prepare data and parameters
  source(file.path(scrdir,"4a_crop.r"), local=TRUE)
  source(file.path(scrdir,"4b_weather_trial.r"), local=TRUE)
  source(file.path(scrdir,"4c_soil.r"), local=TRUE)
  source(file.path(scrdir,"4d_control.r"), local=TRUE)
  ## run model
  source(file.path(scrdir,"5_model.r"), local=TRUE)
  print(h)
}

# compile results
#source(file.path(scrdir,"5_results.r"))

# map results
#source(file.path(scrdir,"6_maps.r"))

# plot results
#source(file.path(scrdir,"7_plots.r"))
