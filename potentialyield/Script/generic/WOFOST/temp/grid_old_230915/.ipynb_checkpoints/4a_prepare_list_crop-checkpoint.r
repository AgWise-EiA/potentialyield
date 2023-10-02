# prepare a list with soil parameters for location i
crop <- wofost_crop(species)

#set phenology parameters
tsumtot<-sum(weath$tmean)
tsum1<-800 #setting tsum1 to 800 degree-days and tsum2 to the remaining degree-days
crop$TSUM1<-tsum1  #Tsum from emergence to anthesis 
crop$TSUM2<-tsumtot-tsum1 #Tsum from anthesis to harvest 

#set canopy parameters
#crop$TDWI<-75 #initial dry weight for winter crops
#crop$SPAN<-37  #life span of leaves at 30 degrees Celsius
#crop$RGRLAI<-0.012 #maximum relative increase in leaf area index

#change plateau
#crop$AMAXTB