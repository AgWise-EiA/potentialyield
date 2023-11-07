#read data
fname<-"~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Rwanda_RAB/Potato/result/geo_4cropModel/temperatureMin_Season_1_PointData_AOI.RDS"
a<-readRDS(fname)

#check topleft corner of data.frame
a[1:10, 1:10]

#check number of rows
nrow(a[,1:2])

#check number of unique coordinates
nrow(unique(a[,1:2]))