sel<-!is.na(srad$Point_1)
a<-srad[sel,]
a<-a[6:nrow(a),]
plot(a$Point_1)
summary(as.numeric(a$Point_1))
