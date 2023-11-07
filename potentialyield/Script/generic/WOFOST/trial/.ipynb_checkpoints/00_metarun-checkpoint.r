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

#do this:
#in the file 2_make_settings.r set; make sure tsum1 and tsum2 from this scropt is not 
#overrun by a default setting by commenting it out. Consider also setting map_results to FALSE, 
# to speed up process 
#prepare
source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))}
source_lines(file="1_run.r", lines=16:25)

#create a data.frame for logging of results
tsum1=100*1:10
tsum2=100*10:20
tsum1=100*1:2
tsum2=100*10:11
test<-expand.grid(tsum1, tsum2)
names(test)<-c("tsum1", "tsum2")

for (l in 1:nrow(test)){
  tsum1<-test$tsum1[l]
  tsum2<-test$tsum2[l]
  
  source_lines(file="1_run.r", lines=26:48)
  test[l, "mae_trial"]<-round(mae(xy.df$ymax, xy.df$wso),1)
  test[l, "mae_agg"]<-round(mae(agg$ymax, agg$wso),1)
  test[l, "e_trial"]<-round(e(xy.df$ymax, xy.df$wso),2)
  test[l, "e_agg"]<-round(e(agg$ymax, agg$wso),2)
  test[l, "r2_trial"]<-round(r2(xy.df$ymax, xy.df$wso),2)
  test[l, "r2_agg"]<-round(r2(agg$ymax, agg$wso),2)
  print(test[l,])
  }

#quick plot of results
qplot(x=test$tsum1,y=test$tsum2, size=test$mae_agg)

#export results
fname<-file.path(outdir1, outdir2, "test.txt")
write.table(test, fname, row.names = F, sep = "t")
