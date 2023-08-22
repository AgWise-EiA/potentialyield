#more reading: https://reagro.org/methods/explanatory/wofost/soil.html


# prepare a list with soil parameters for location i
soil <- wofost_soil('ec1')

listRaster_soil <-list.files(path="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Soil/iSDA", pattern=".tif$")

# vol..soil.moisture.content.as.function.of.pF
soil$SMTAB 

# soil moisture content at wilting point
soil$SMW 

# soil moisture content at field capacity
soil$SMFCF

# soil moisture content at saturation
soil$SM0

# critical soil air content for aeration
soil$CRAIRC

# 10-log hydraulic conductivity as function of pF
soil$CONTAB

# hydraulic conductivity of saturated soil
soil$K0

# maximum percolation rate root zone
soil$SOPE

# maximum percolation rate subsoil
soil$KSUB

# Maximum surface storage
soil$SSMAX

# groundwater present
soil$IZT

# Rain infiltration as function of storm size (0/1)
soil$IFUNRN

# presence (1) or absence (0) of drains
soil$IDRAIN

# initial amount of water in excess of wilting point, but not exceeding field capacity
soil$WAV

# initial depth of groundwater table
soil$ZTI

# effective depth of drains (drainage base)
soil$DD

# maximum rooting depth of the soil
soil$RDMSOL

# on-infiltrating fraction of rainfall (FUNRAI=0) or maximum non-infiltrating fraction (FUNRAI=1)
soil$NOTINF

# Initial surface storage
soil$SSI

# Limiting amount of volumetric moisture in upper soil layer
soil$SMLIM

