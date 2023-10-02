## create a list with soil data and parameters
##more info: https://reagro.org/methods/explanatory/wofost/soil.html

# prepare a list with standard soil parameters for current site
soil <- wofost_soil("ec1")

#get soildata for current site
sel<-soildata$ID==siteh
soildatah<-soildata[sel,]

# set soil parameters 
## soil moisture content at wilting point
## one value required, decided to use average for top meter
soil$SMW<- 0.01*(5*soildatah$"PWP_0-5cm" + 
    10*soildatah$"PWP_5-15cm" + 
    15*soildatah$"PWP_15-30cm" + 
    30*soildatah$"PWP_30-60cm" + 
    40*soildatah$"PWP_60-100cm")

## soil moisture content at field capacity
## one value required, decided to use average for top meter
soil$SMFCF<- 0.01*(5*soildatah$"FC_0-5cm" + 
10*soildatah$"FC_5-15cm" + 
  15*soildatah$"FC_15-30cm" + 
  30*soildatah$"FC_30-60cm" + 
  40*soildatah$"FC_60-100cm")

## soil moisture content at saturation
## one value required, decided to use average for top meter
soil$SM0<- 0.01*(5*soildatah$"SWS_0-5cm" + 
10*soildatah$"SWS_5-15cm" + 
  15*soildatah$"SWS_15-30cm" + 
  30*soildatah$"SWS_30-60cm" + 
  40*soildatah$"SWS_60-100cm")

## hydraulic conductivity of saturated soil (cm day-1)
## one value required, decided to use average for top meter
## the factor 24 converts from "per hour" to "per day")
## the factor 0.1 converts from "mm hour" to "cm")
soil$K0<-0.1*24*0.01*(5*soildatah$"KS_0-5cm" + 
                 10*soildatah$"KS_5-15cm" + 
                 15*soildatah$"KS_15-30cm" + 
                 30*soildatah$"KS_30-60cm" + 
                 40*soildatah$"KS_60-100cm")

## presence (1) or absence (0) of drains
soil$IDRAIN<-0

## initial amount of water in excess of wilting point, but not exceeding field capacity
soil$WAV<-50

# initial depth of groundwater table
# not available, decided to use standard the standard value for the ec1 soil
soil$ZTI<-150

# maximum rooting depth of the soil
soil$RDMSOL<-150

#non-infiltrating fraction of rainfall (FUNRAI=0) or maximum non-infiltrating fraction (FUNRAI=1)
soil$NOTINF<-0

# Initial surface storage
soil$SSI<-0

# Limiting amount of volumetric moisture in upper soil layer
soil$SMLIM<-1

