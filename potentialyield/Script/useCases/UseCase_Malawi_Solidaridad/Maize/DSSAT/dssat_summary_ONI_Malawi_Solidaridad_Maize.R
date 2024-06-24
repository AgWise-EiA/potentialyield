#################################################################################################################
## source "dssat_summary_ONI.R" functions and execute it for Kenya KALRO Beans use case
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/Testing/dssat_summary_ONI_v3.R")
country="Malawi"
useCaseName="Solidaridad"
Crop = "Maize"
Extent = "AOI"
Season = 1
Plot = TRUE

# Step 1 : Aggregate the DSSAT output
merge_DSSAT_output(country, useCaseName, Crop, Extent, Season)
# Step 2 : Add the ONI to aggregate DSSAT output
get_ONI(country, useCaseName, Crop, Extent, Season, Plot)