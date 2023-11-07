#do a test to assess time it will take
t0<-Sys.time()
cores<- max(detectCores()-10, 1) #minus ten , not to overload server
m<-matrix(20:120)
a<-dapply(X=m, FUN=f, MARGIN = 1, parallel = TRUE, mc.cores = cores,
          return = "matrix", drop = TRUE)
t1<-Sys.time()
tdiff<-as.numeric((t1-t0))
ttot<-round(tdiff*nrow(xy.df)*0.01/60,2)
print(paste0("this will take ",ttot," minutes (",round(ttot/60,2)," hours)"))