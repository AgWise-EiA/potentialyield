# Merge output of the APSIM simulations for diverse locations

#################################################################################################################
## sourcing required packages
#################################################################################################################
packages_required <- c("arrow","mgsub","tidyverse","future.apply","future","furrr")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}


read_APSIM_weather_file <- function(filepath) {
  # Read all lines
  lines <- readLines(filepath)
  
  # Extract latitude and longitude
  lat_line <- grep("^latitude", lines, value = TRUE)
  long_line <- grep("^longitude", lines, value = TRUE)
  
  if (length(lat_line) == 0 || length(long_line) == 0) {
    stop("Could not find latitude/longitude in file")
  }
  
  lat <- as.numeric(trimws(strsplit(lat_line, "=")[[1]][2]))
  long <- as.numeric(trimws(strsplit(long_line, "=")[[1]][2]))
  
  
  # Find the line with column names
  header_idx <- grep("^year\\s+day", lines)
  if (length(header_idx) == 0) {
    stop("Could not find weather data header in file")
  }
  
  # Extract header line and clean column names
  colnames <- strsplit(trimws(lines[header_idx]), "\\s+")[[1]]
  
  # Extract the data (skip the units row)
  data_lines <- lines[(header_idx + 2):length(lines)]
  
  # Read into a data frame
  df <- read.table(
    text = paste(data_lines, collapse = "\n"),
    header = FALSE
  )
  
  # Assign correct column names
  names(df) <- colnames
  
  # Compute actual calendar date from year + day-of-year
  df$date <- as.Date(paste(df$year, df$day), format = "%Y %j")
  df$latitude <- lat
  df$longitude <- long
  
  
  return(df)
}


# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))

#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param season when data is needed for more than one season, this needs to be provided to be used in the file name
#' @param varietyids ids of the varieties based on the cultivar file of APSIM (column @VAR# in the cultivar file and parameter INGENO in the experimental file *.**X)
#' @param zone_folder When TRUE the output folders are organized by administrative level 1.
#' @param level2_foler When TRUE the output folders are organized by administrative level 2 (has to be part of the administrative level 1 or "zone" of the country) 
#'        for the specific location the experimental file is created
#'        
#' @return merged results from APSIM in RDS format
#'
#' @examples merge_APSIM_output(country="Rwanda", useCaseName="RAB",Crop="Maize",varietyids=c("Early")

merge_APSIM_output <- function(country, useCaseName, Crop, AOI = FALSE, season = NULL, varietyids, zone_folder = TRUE, level2_folder = FALSE) {
  
  # Set up parallel processing
  num_cores <- availableCores() -3
  plan(multisession, workers = num_cores)
  all_results <- NULL
  
  for (varietyid in varietyids) {
    if (AOI == TRUE) {
      if (is.null(season)) {
        stop("With AOI=TRUE, season cannot be null. Please provide a season number.")
      }
      path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName, "/", Crop, "/transform/APSIM/AOI/", varietyid, sep = "")
    } else {
      path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName, "/", Crop, "/transform/APSIM/fieldData/", varietyid, sep = "")
    }
    
    if (!dir.exists(file.path(path.to.extdata))) {
      message("Experiments not found for varietyid: ", varietyid, ". Process stopped for this varietyid.")
      next
    }
    setwd(path.to.extdata)
    
    a <- list.files(path = path.to.extdata, pattern = "^HarvestReport_.*\\.parquet$$", include.dirs = TRUE, full.names = TRUE, recursive = TRUE)
    b <- list.files(path = path.to.extdata, pattern = "^wth.*\\.met$", include.dirs = TRUE, full.names = TRUE, recursive = TRUE)
    
    results <- future_map2_dfr(a,b, function(.x,.y) {
      tryCatch({
        file <- read_parquet(.x)
        file$file_name <- .x
        
        if (level2_folder == TRUE & zone_folder == TRUE) {
          test <- mgsub(.x, c(path.to.extdata, "/EXTE.*"), c("", ""))
          test <- strsplit(test, "/")[[1]]
          test <- test[test != ""]
          file$zone <- test[1]
          file$Loc <- file$zone
          file$level2 <- test[2]
        } else if (level2_folder == FALSE & zone_folder == TRUE) {
          file$zone <- mgsub(.x, c(path.to.extdata, "/", "/EXTE.*"), c("", "", ""))
          file$Loc <- file$zone
          file$level2 <- NA
        } else if (level2_folder == TRUE & zone_folder == FALSE) {
          stop("You need to define first a zone (administrative level 1) to be able to run the model for level 2 (administrative level 2). Process stopped")
        } else {
          file$zone <- NA
          file$level2 <- NA
          file$Loc <- NA
        }
        file$Variety <- varietyid
        
        wht_file <- read_APSIM_weather_file(.y)
        # file$Lat <- unique(wht_file$LAT)
        # file$Long <- unique(wht_file$LONG)
        file$latitude <- unique(wht_file$latitude)
        file$longitude <- unique(wht_file$longitude)
        file
      }, error = function(e) {
        cat("Error processing file:", .x, "\n", e$message, "\n")
        NULL
      })
    })
    all_results <- bind_rows(all_results, results)
  }
  
  if (AOI == TRUE) {
    dir_path <- paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName, "/", Crop, "/result/APSIM/AOI/")
  } else {
    dir_path <- paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName, "/", Crop, "/result/APSIM/fieldData/")
  }
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  if (AOI) {
    write_parquet(
      all_results,
      sink = paste0(
        dir_path,
        "useCase_", country, "_", useCaseName, "_", Crop, "_AOI_season_",
        season, ".parquet"
      )
    )
  } else {
    write_parquet(
      all_results,
      sink = paste0(
        dir_path,
        "useCase_", country, "_", useCaseName, "_", Crop, "_fieldData_season_",
        season, ".parquet"
      )
    )
  }
}
