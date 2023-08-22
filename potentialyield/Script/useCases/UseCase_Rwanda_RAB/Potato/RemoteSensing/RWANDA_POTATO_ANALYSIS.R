### ************************** ###

## Amit Srivastava, IRRI SOuth Asia Regional Centre, Varanasi, India


library(plotly)
library(raster)
library(rgdal)
library(gridExtra)
library(sp)
library(ggplot2)
library(caret)
library(signal)
library(timeSeries)
library(zoo)
library(pracma)
library(rasterVis)
library(RColorBrewer)
library(dplyr)
library(reshape2) 
#install.packages("reshape") 
library(reshape) 

setwd("read the wroking directory") ## define your working directory here


files <- list.files(".\\PD_RW_S2", pattern='tif$', ##reading phenological raster files
                    full.names = TRUE) # list all the raster
files
PD <- raster::stack(files)

#Loading the potato area mask
Pmask <- readOGR("C:\\Projects\\Crop phenology\\CROP_TYPE_VEC\\potato2.shp")
plot(Pmask)


## crop and mask cropland
adsm <- crop(PD, Pmask)
adsm <- mask(PD, Pmask)
plot(PD)
names(adsm)<- c("Y2011_12","Y2012_13","Y2013_14","Y2014_15","Y2015_16","Y2016_17","Y2017_18","Y2018_19","Y2019_20","Y2020_21")
names(adsm)
plot(adsm)
dir.create("2020_21")
#writeRaster(adsm1,".//2020_21//2020_21_S1", bylayer=TRUE,format="GTiff", overwrite=T)


#########################################################################
#Load the duration files
D1 <- list.files("reda the working directory", pattern='tif$', ##reading geotiff files which are kept in 2021-22 folder
                    full.names = TRUE) # list all the raster
D1
D <- raster::stack(D1) ## stack the rasters
plot(D)
names(D)  

D_70_120 <- calc(D, fun=function(x){ x[x >=70 & x <= 125] <- 2; return(x)}) # values between 70 to 125 is Length of growth duration
s2 <- calc(D_70_120, fun=function(x){ x[x < 2] <- NA; return(x)}) # remove -ve values
spplot(s2)  


## crop and mask acc to duration
adsm_s2 <- crop(adsm, s2)
adsm_s2 <- mask(adsm, s2)
plot(adsm_s2$Y2012_13)
adsm_s2


## extract mean values of polygons
poly_ras_extract <- raster::extract(adsm_s2, Pmask, fun=mean,na.rm=TRUE, sp = T)
class(poly_ras_extract)

dir.create("intermediate_files3")
write.csv(poly_ras_extract, ".\\intermediate_files3\\ps2.csv") ## save csv
##### MAKING BOX PLOTs #####################

## read csv
mg_vh1 <- read.csv(".\\intermediate_files3\\ps2.csv")
names(mg_vh1)
View(mg_vh1)

mg_vh3 <- melt(mg_vh1, id.vars=c("X"))## melt the data
mg_vh3
View(mg_vh3)
str(mg_vh3)
mg_vh4 <- na.omit(mg_vh3)
summary(mg_vh4)
View(mg_vh4)
names(mg_vh4)

#mg_vh4$variable
#write.csv(mg_vh4, ".\\intermediate_files3\\ps_df.csv")
## second data set on a very different scale

Means <- mg_vh4 %>% group_by(variable) %>% 
  summarize(Avg = mean(value))
View(Means)

## Plot the diagram
bp<-ggplot(mg_vh4, aes(x = variable, y = value )) +
  geom_boxplot(fill="light green")+
  theme(axis.text.x = element_text(angle = 90))+ theme(legend.position="top")+ labs(title= "Potato planting dates (Feb-Aug)",x="Year", y="Julian dates")+ theme_classic()
bp
bp1<-bp+
  geom_point(data = Means, mapping = aes(x = variable, y = Avg)) + 
  geom_line(data = Means, mapping = aes(x = variable, y = Avg,group=1), color= "red")+ theme(text=element_text(size=20))
bp1
bp2<- bp1 + geom_text(data=Means, aes(label=Avg),nudge_x = 0.25, nudge_y = 0.25,check_overlap = T)
bp2


################# To extract co-ordinates, add cellnumbers = T############################################

extract<- raster::extract(adsm_s2, Pmask, na.rm=TRUE,cellnumbers = T,df=TRUE)
extract <- extract[order(extract$cell),]
# Extract coordinates
xy <- xyFromCell(adsm_s2, cell = extract$cell, spatial = FALSE)

# Convert to df and add cellnumber
xy <- as.data.frame(xy)
xy$cell <- extract$cell

# Merge two data frames
extract_end <- merge(extract, xy)
extract_end <- extract_end[order(extract_end$cell),]
View(extract_end)
extract_end <- extract_end[,-2]
extract_end_m <- melt(extract_end, id.vars=c("cell","x","y"))
View(extract_end_m)
extract_end_na <- na.omit(extract_end_m)
names(extract_end_m)
write.csv(extract_end_na,".\\intermediate_files3\\S2_COORDINATE.csv")