#################################################################################################################
## source "dssat_summary_ONI.R" functions and execute it for Kenya KALRO Maize use case
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/dssat_summary_ONI.R")
country="Kenya"
useCaseName="KALRO"
Crop = "Maize"
Extent = "AOI"
Season = 1
Plot = TRUE

# Step 1 : Aggregate the DSSAT output
merge_DSSAT_output(country, useCaseName, Crop, Extent, Season)
# Step 2 : Add the ONI to aggregate DSSAT output
get_ONI(country, useCaseName, Crop, Extent, Season, Plot)