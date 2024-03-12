

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/dssat_exec.R")
prov <- list.files('~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Kenya_KALRO/Maize/transform/DSSAT/AOI/KY0012')

for (i in 1:length(prov)){
  print(prov[i])
  execmodel_AOI <-dssat.exec(country = "Kenya",  useCaseName = "KALRO", Crop = "Maize",
                             AOI = TRUE,TRT=1:8,ingenoid="999992",Province = prov[i])
} 
