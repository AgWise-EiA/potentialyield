#create a new folder if it does not exist
wdif<-function(x){if(!dir.exists(x))dir.create(x)}

#do a testrum
timetest<-function(cores){
  m<-matrix(20:120)
  t0<-Sys.time()
  a<-dapply(X=m, FUN=f, MARGIN = 1, parallel = TRUE, mc.cores = cores,
          return = "matrix", drop = TRUE)
  t1<-Sys.time()
  tdiff<-as.numeric((t1-t0))
  ttot<-round(tdiff*nrow(xy.df)*0.01/60,2)
  print(paste0("this will take ",ttot," minutes (",round(ttot/60,2)," hours)"))
}
