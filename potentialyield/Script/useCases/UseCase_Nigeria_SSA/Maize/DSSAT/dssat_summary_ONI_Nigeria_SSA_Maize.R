#################################################################################################################
## source "dssat_summary_ONI.R" functions and execute it for Kenya KALRO Beans use case
#################################################################################################################

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/Testing/dssat_summary_ONI_v3.R")
country="Nigeria"
useCaseName="SSA"
Crop = "Maize"
Extent = "AOI"
Season = 1
Plot = TRUE

# Step 1 : Aggregate the DSSAT output
merge_DSSAT_output(country, useCaseName, Crop, Extent, Season)
# Step 2 : Add the ONI to aggregate DSSAT output
get_ONI(country, useCaseName, Crop, Extent, Season, Plot)

# Step 3 : Get the summary plots
summary_plots(country = country, useCaseName = useCaseName, Crop = Crop, Extent = Extent, Season = Season, Plot = Plot)