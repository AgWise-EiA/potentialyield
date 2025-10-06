country <- "Rwanda"
variety <- "Early"
useCaseName <- "RAB"
Crop <- "Maize"

# Settings for first part
produce_EXTE_plots <- FALSE  # TRUE for producing scatter plots for each EXTE
yield_column <- NULL # Change this to change column for yield (otherwise Yield is used)
varietyids = c("Early")

# Settings for second part
zone_folder <- TRUE
level2_folder <- FALSE
AOI <- TRUE
season <- 1
short_variety = "Early"  # Name of the short variety
medium_variety = "Early"  # Name of the medium variety
long_variety = "Early"  # Name of the long variety


source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/APSIM/05_produce_APSIM_plots.R")
results <- read_parquet("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Rwanda_RAB/Maize/result/APSIM/AOI/useCase_Rwanda_RAB_Maize_AOI_season_1.parquet")


for (variety in varietyids){
  apsim.plots(results = results,
              country = country,
              variety = variety,
              useCase = useCaseName,
              Crop = Crop,
              produce_EXTE_plots = produce_EXTE_plots,
              yield_column = yield_column)
  }




source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/APSIM/06_APSIM_summary_ONI.R")

get_ONI(country, useCaseName, Crop, AOI=TRUE, season, Plot=TRUE,
        short_variety = "Early", medium_variety = "Early", long_variety = "Early",
        justplot=FALSE)

