
#######################################
# 1. Get the potato data from SAnDMan #
#######################################

source("~/agwise/AgWise_Scripts/data_sourcing/get_ONA_data.R")

require("plyr")
require("dplyr")

#downloading the data
user <- "user" #replace by your ONA username
pw   <- "pw"   #replace by your ONA password

#get the list of all datasets of user...
dss <- findONAdatasets(user = user, pw = pw)

#download and decompose the assign field/trial/plot data:
id <- dss[dss$id_string == "Assign_FDTLPO",]$id
ad <- getONAdata(user = user, pw = pw, id = id) 
ad <- decomposeONAdata(ad)

#get the field identifiers
af <- ad[[1]] %>%
  dplyr::filter(grepl("FD", entity)) %>%
  dplyr::select(FDID2_new, FD_name_new, FD_owner, HHID, lat, lon) %>%
  dplyr::rename(FDID2 = FDID2_new,
                FD_name = FD_name_new)

#get the trial identifiers
at <- ad[[3]] %>%
  dplyr::left_join(ad[[1]] %>% dplyr::select(L1, entity, season, plantingDate, expCode)) %>%
  dplyr::filter(grepl("TL", entity),
                L2 == "trial") %>%
  dplyr::select(TLID2_new, TL_name_new, season, plantingDate, expCode) %>%
  dplyr::mutate(plantingDate = as.Date(plantingDate, format="%Y-%m-%d")) %>%
  dplyr::rename(TLID2 = TLID2_new,
                TL_name = TL_name_new)

#download and decompose the potato plot level data:
id <- dss[dss$id_string == "Measure_Potato_PO",]$id
pd <- getONAdata(user = user, pw = pw, id = id) 
pd <- decomposeONAdata(pd)

#get the potato plot yield data and merge with trial and field identifiers:
ds1 <- pd[[3]] %>% #plot level data
  dplyr::filter(!is.na(tubersFW) | !is.na(tubersMarketableFW)) %>%
  dplyr::left_join(pd[[1]] %>% dplyr::select(L1, projectCode, FDID2, TLID2, today, start)) %>% #field level data
  dplyr::mutate(harvestDate = as.Date(today, format="%Y-%m-%d"),
                start = as.POSIXct(gsub("\\+.*","", start), format="%Y-%m-%dT%H:%M:%S", tz="UTC")) %>%
  dplyr::select(projectCode, FDID2, TLID2, POID2, POID2_label, start, harvestDate, plotLength, plotWidth, nrPlants, tubersFW, tubersMarketableFW) %>%
  dplyr::left_join(af) %>%
  dplyr::left_join(at)

#extracting treatment from label
ds1 <- ds1 %>% 
  dplyr::mutate(treat = sub("_[^_]+$", "", POID2_label),
                treat = gsub("_rep1", "", treat),
                treat = gsub("_rep2", "", treat),
                treat = gsub("_repA", "", treat),
                treat = gsub("_repB", "", treat),
                plotSize = as.numeric(plotLength) * as.numeric(plotWidth),
                tubersFW = as.numeric(tubersFW),
                tubersMarketableFW = as.numeric(tubersMarketableFW),
                plantingDate = as.Date(plantingDate, format = "%Y-%m-%d")) %>%
  dplyr::filter(treat != "",
                expCode != "RS-PLR-1") #removing lime trials without varying NPK rates

saveRDS(ds1, "~/agwise/AgWise_Data/data_sourcing/UseCase_Rwanda_RAB/Potato/result/SAnDMan-potato-fieldData.RDS")
saveRDS(ds1, "~/agwise/AgWise_Data/fieldData_analytics/UseCase_Rwanda_RAB/Potato/raw/SAnDMan-potato-fieldData.RDS")


###################################
# 2. Get the RwaSIS season 1 data #
###################################

ds2 <- read.csv("~/agwise/AgWise_Data/data_sourcing/UseCase_Rwanda_RAB/Potato/raw/fieldData/rwasis-potato-fertiliser-2022A-data.csv")

ds2 <- ds2 %>%
  dplyr::mutate(expCode = "RS-PFR-1",
                plantingDate = as.Date(planting_date, format="%Y-%m-%d"),
                harvestDate = as.Date(harvest_date, format="%Y-%m-%d"))

saveRDS(ds2, "~/agwise/AgWise_Data/data_sourcing/UseCase_Rwanda_RAB/Potato/result/RwaSIS_potato_2022A_fieldData.RDS")
saveRDS(ds2, "~/agwise/AgWise_Data/fieldData_analytics/UseCase_Rwanda_RAB/Potato/raw/RwaSIS_potato-2022A-fieldData.RDS")

#####################################
# 3. Preparing the IFDC potato data #
#####################################

ds3 <- read.csv("~/agwise/AgWise_Data/data_sourcing/UseCase_Rwanda_RAB/Potato/raw/fieldData/IFDC_Rwanda potato 2014B season data subset.csv")
ds3_nutrates <- read.csv("~/agwise/AgWise_Data/data_sourcing/UseCase_Rwanda_RAB/Potato/raw/fieldData/IFDC_Rwanda potato 2014B season treat nutrates.csv")

ds3 <- ds3 %>%
  gather(treat, TY, control:all_redK) %>%
  dplyr::mutate(season = "2014B",
                expCode = "IFDC",
                plantingDate = NA,
                harvestDate = NA) %>%
  left_join(ds3_nutrates)

saveRDS(ds3, "~/agwise/AgWise_Data/data_sourcing/UseCase_Rwanda_RAB/Potato/result/IFDC_potato_2014B_fieldData.RDS")
saveRDS(ds3, "~/agwise/AgWise_Data/fieldData_analytics/UseCase_Rwanda_RAB/Potato/raw/IFDC_potato-2014B-fieldData.RDS")
