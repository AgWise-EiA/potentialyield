#################################################################################################################
## Create experimental data in DSSAT format
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/dssat_expfile.R")
# expdata <- dssat.expfile(country = "Zambia",  useCaseName = "Solidaridad", Crop = "Soybean", AOI = TRUE, filex_temp="ESAC8501.SBX", Planting_month_date = NULL,Harvest_month_date=NULL,ID="TLID",season =NULL, plantingWindow=NULL)
#detach("package:DSSAT", unload = TRUE)
#devtools::install_github("palderman/DSSAT", ref = "develop",force=T,upgrade = 'always')
#library('DSSAT')
#test <- DSSAT::read_sol("SOIL.SOL", id_soil = "TRAN00001")
country <- "Kenya"
countryShp <- geodata::gadm(country, level = 2, path='.')
prov <- unique(countryShp$NAME_1)
prov <- list.files('~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Kenya_KALRO/Maize/transform/DSSAT/AOI/KY0012')
varieties <- c("999991","999992","KY0012")
# Crop = "Maize"; 
# AOI = TRUE; filex_temp="KEAG8104.MZX"; Planting_month_date="03-01"; 
# Harvest_month_date="07-30"; ID="TLID";season =1; plantingWindow=7; 
# ingenoid="999991";Province = prov[i]

for (j in 1:length(varieties)){
  for (i in 1:length(prov)){
  expdata_AOI <- dssat.expfile(country = "Kenya",  useCaseName = "KALRO", Crop = "Maize", 
                               AOI = TRUE, filex_temp="KEAG8104.MZX", Planting_month_date="03-01", 
                               Harvest_month_date="07-30", ID="TLID",season =1, plantingWindow=7, 
                               ingenoid=varieties[j],Province = prov[i])
  } 
}


#################################################################################################################
## Run the DSSAT model
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/dssat_exec.R")
for (j in 1:length(varieties)){
  for (i in 2:length(prov)){
    print(prov[i])
    execmodel_AOI <-dssat.exec(country = "Kenya",  useCaseName = "KALRO", Crop = "Maize",
                               AOI = TRUE,TRT=1:8,ingenoid=varieties[j],Province = prov[i])
  }
}

setwd("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Kenya_KALRO/Maize/transform/DSSAT/AOI/999991/Embu/EXTE0001/")
run_dssat(file_name="DSSBatch.v48",suppress_output = TRUE)

#################################################################################################################
## Merge results
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/merge_DSSAT_output.R")
# mergeresults <-merge_DSSAT_output(country = "Kenya",  useCaseName = "KALRO", Crop = "Maize", AOI = TRUE,season=1)
mergeresults_AOI <-merge_DSSAT_output(country = "Kenya",  useCaseName = "KALRO", Crop = "Maize", AOI = TRUE,season=1)
