# create a list with crop parameters
##more info: https://reagro.org/methods/explanatory/wofost/crop.html
## wofost_crop(species, describe=TRUE)

#get predefined parameters
crop <- wofost_crop(species)

#set phenology parameters
if(exists("tsum1"))crop$TSUM1<-tsum1 #tsum from emergence to anthesis 
if(exists("tsum2"))crop$TSUM2<-tsum2 #tsum from anthesis to maturity 

#set canopy parameters
#crop$TDWI<-75 
#crop$SPAN<-100  #life span of leaves at 35 degrees Celcius
#crop$RGRLAI<-0.012 #maximum relative increase in LAI

#set plateau
#crop$AMAXTB<- change the plateau