#################################################################################################################
## Create experimental data in DSSAT format
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/Testing/dssat_expfile_provs_testing_V5.R")

prov <- list.files('~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Kenya_KALRO/Maize/transform/DSSAT/AOI/999993')

varieties <- c("999993","999994","999995")

for (j in 1:length(varieties)){
  for (i in 1:length(prov)){
    expdata_AOI <- dssat.expfile(country = "Kenya",  useCaseName = "KALRO", Crop = "Maize",
                                 AOI = TRUE, filex_temp="KEAG8104.MZX", Planting_month_date="03-01",
                                 Harvest_month_date="11-30", ID="TLID",season =1, plantingWindow=12,
                                 ingenoid=varieties[j],Province = prov[i])
  }
}

#################################################################################################################
## Run the DSSAT model
#################################################################################################################

prov <- list.files('~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Kenya_KALRO/Maize/transform/DSSAT/AOI/999993/')
varieties <- c("999993","999994","999995")

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/Testing/dssat_exec.R")
for (j in 1:length(varieties)){
  for (i in 1:length(prov)){
    execmodel_AOI <-dssat.exec(country = "Kenya",  useCaseName = "KALRO", Crop = "Maize",
                               AOI = TRUE,TRT=1:13,ingenoid=varieties[j],Province = prov[i])
  }
}


#################################################################################################################
## Merge results
#################################################################################################################

#################################################################################################################
## source "dssat_summary_ONI.R" functions and execute it for Kenya KALRO Beans use case
#################################################################################################################
# 
# source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/Testing/dssat_summary_ONI_v3.R")
# country="Kenya"
# useCaseName="KALRO"
# Crop = "Maize"
# Extent = "AOI"
# Season = 1
# Plot = TRUE

# # Step 1 : Aggregate the DSSAT output
# merge_DSSAT_output(country, useCaseName, Crop, Extent, Season)
# # Step 2 : Add the ONI to aggregate DSSAT output
# get_ONI(country, useCaseName, Crop, Extent, Season, Plot)
# 


