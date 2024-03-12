#################################################################################################################
## Run the DSSAT model
#################################################################################################################

country <- "Kenya"
Crop = "Beans"

prov <- list.files('~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Kenya_KALRO/Beans/transform/DSSAT/IB2019')
experiments <- c("KEAG0001","KEAG0002","KEAG0003")

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/Testing/dssat_exec_zone.R")

  for (i in 20:length(prov)){
    execmodel_AOI <-dssat.exec(country = "Kenya",  useCaseName = "KALRO", Crop = "Beans",
                               AOI = TRUE,TRT=1:5,ingenoid="IB2019",Province = prov[i],experiments=experiments,model="GRO")
  }


# setwd("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Kenya_KALRO/Maize/transform/DSSAT/AOI/999991/Embu/EXTE0001/")
# run_dssat(file_name="DSSBatch.v48",suppress_output = TRUE)
