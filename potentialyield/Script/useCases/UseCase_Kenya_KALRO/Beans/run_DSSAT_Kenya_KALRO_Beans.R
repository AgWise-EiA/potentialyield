#################################################################################################################
## Create experimental data in DSSAT format
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/Testing/dssat_expfile_provs_testing_V5.R")

prov <- list.files('~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Kenya_KALRO/Beans/transform/DSSAT/AOI/IB2022')
varieties <- c("IB2022")

for (j in 1:length(varieties)){
  for (i in 23:length(prov)){
    expdata_AOI <- dssat.expfile(country = "Kenya",  useCaseName = "KALRO", Crop = "Beans",
                                 AOI = TRUE, filex_temp="KEAG0004.BNX", Planting_month_date="03-01",
                                 Harvest_month_date="07-30", ID="TLID",season =1, plantingWindow=16,
                                 ingenoid=varieties[j],Province = prov[i])
  }
}

#################################################################################################################
## Run the DSSAT model
#################################################################################################################

prov <- list.files('~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Kenya_KALRO/Beans/transform/DSSAT/AOI/IB2022')
varieties <- c("IB2022")

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/Testing/dssat_exec_bn.R")
for (j in 1:length(varieties)){
  for (i in 23:length(prov)){
    execmodel_AOI <-dssat.exec(country = "Kenya",  useCaseName = "KALRO", Crop = "Beans",
                               AOI = TRUE,TRT=1:17,ingenoid=varieties[j],Province = prov[i])
  }
}

#################################################################################################################
###################################################################################################################

#################################################################################################################
## source "dssat_summary_ONI.R" functions and execute it for Kenya KALRO Beans use case
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/Testing/dssat_summary_ONI_v3.R")
country="Kenya"
useCaseName="KALRO"
Crop = "Beans"
Extent = "AOI"
Season = 1
Plot = TRUE

# Step 1 : Aggregate the DSSAT output
merge_DSSAT_output(country, useCaseName, Crop, Extent, Season)
# Step 2 : Add the ONI to aggregate DSSAT output
get_ONI(country, useCaseName, Crop, Extent, Season, Plot)














# #################################################################################################################
# ## Create experimental data in DSSAT format
# #################################################################################################################
# 
# source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/Testing/dssat_expfile_zone.R")
# # expdata <- dssat.expfile(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Beans", AOI = TRUE, filex_temp="ESAC8501.SBX", Planting_month_date = NULL,Harvest_month_date=NULL,ID="TLID",season =NULL, plantingWindow=NULL)
# #detach("package:DSSAT", unload = TRUE)
# #devtools::install_github("palderman/DSSAT", ref = "develop",force=T,upgrade = 'always')
# #library('DSSAT')
# #test <- DSSAT::read_sol("SOIL.SOL", id_soil = "TRAN00001")
# country <- "Kenya"
# 
# prov <- list.files('~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Kenya_KALRO/Beans/transform/DSSAT/IB2019')
# experiments <- c("KEAG0001.BNX","KEAG0002.BNX","KEAG0003.BNX")
# Planting_month_date <- "03-01" ## the earliest possible plating mm-dd
# Harvest_month_date <- "07-30" ## the earliest harvest date in mm-dd
# #varieties <- c("999991","999992","KY0012")
# # Crop = "Maize"; 
# # AOI = TRUE; filex_temp="KEAG8104.MZX"; Planting_month_date="03-01"; 
# # Harvest_month_date="07-30"; ID="TLID";season =1; plantingWindow=7; 
# # ingenoid="999991";Province = prov[i]
# 
# for (i in 1:length(prov)){
#   for (j in 1:length(experiments)){
#     print(prov[i])
#     print(experiments[j])
#   expdata_AOI <- dssat.expfile(country = "Kenya",  useCaseName = "KALRO", Crop = "Beans", 
#                                AOI = TRUE, filex_temp=experiments[j], Planting_month_date=Planting_month_date, 
#                                Harvest_month_date=Harvest_month_date, ID="TLID",season =1, plantingWindow=4, 
#                                ingenoid="IB2019",Province = prov[i],fertilizer=T)
#   } 
# }

# #################################################################################################################
# ## Merge results
# #################################################################################################################
# 
# # source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/merge_DSSAT_output.R")
# # mergeresults_AOI <-merge_DSSAT_output(country = "Kenya",  useCaseName = "KALRO", Crop = "Maize", AOI = TRUE,season=1)
# 
# source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/merge_DSSAT_output.R")
# # mergeresults <-merge_DSSAT_output(country = "Kenya",  useCaseName = "KALRO", Crop = "Maize", AOI = TRUE,season=1)
# for (j in 1:length(varieties)){
#   for (i in 1:length(prov)){
# mergeresults_AOI <-merge_DSSAT_output(country = "Kenya",  useCaseName = "KALRO", Crop = "Maize",
#                                       AOI = TRUE,season=1,ingenoid=varieties[j],Province = prov[i])
#   }
# }






