# #################################################################################################################
# ## Create experimental data in DSSAT format
# #################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/Testing/dssat_expfile_provs_testing_V2.R") #with high initial soil moisture.

prov <- list.files('~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Nigeria_SSA/Maize/transform/DSSAT/AOI/234001')
varieties <- c("234001")

for (j in 1:length(varieties)){
  for (i in 1:length(prov)){
    expdata_AOI <- dssat.expfile(country = "Nigeria",  useCaseName = "SSA", Crop = "Maize",
                                 AOI = TRUE, filex_temp="KEAG8104.MZX", Planting_month_date="03-01",
                                 Harvest_month_date="11-30", ID="TLID",season =1, plantingWindow=29,
                                 ingenoid=varieties[j],Province = prov[i])
  }
}

# #################################################################################################################
# ## Run the DSSAT model
# #################################################################################################################

prov <- list.files('~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Nigeria_SSA/Maize/transform/DSSAT/AOI/234001/')
varieties <- c("234001")

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/Testing/dssat_exec.R")
for (j in 1:length(varieties)){
  for (i in 1:length(prov)){
    execmodel_AOI <-dssat.exec(country = "Nigeria",  useCaseName = "SSA", Crop = "Maize",
                               AOI = TRUE,TRT=1:30,ingenoid=varieties[j],Province = prov[i])
  }
}


# #################################################################################################################
# ## Merge results
# #################################################################################################################
# #################################################################################################################
# ## source "dssat_summary_ONI.R" functions and execute it for Kenya KALRO Beans use case
# #################################################################################################################
# 
# # source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/Testing/dssat_summary_ONI_v2.R")
# source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/Testing/dssat_summary_ONI_v3.R")
# country="Nigeria"
# useCaseName="SSA"
# Crop = "Maize"
# Extent = "AOI"
# Season = 1
# Plot = TRUE
# 
# # Step 1 : Aggregate the DSSAT output
# merge_DSSAT_output(country, useCaseName, Crop, Extent, Season)
# # Step 2 : Add the ONI to aggregate DSSAT output
# get_ONI(country, useCaseName, Crop, Extent, Season, Plot)
# 
# 
# 
# 
# 
# 
# 
