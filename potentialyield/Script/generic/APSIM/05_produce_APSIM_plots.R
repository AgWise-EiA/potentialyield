# Sourcing required packages -------------------------------------------
packages_required <- c("ggplot2", "tidyverse", "ggridges", "patchwork", "Rmisc", 
                       "terra", "cowplot", "foreach", "doParallel", "future",
                       "future.apply", "sf", "arrow", "geodata")

installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])
}

# load required packages
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))


apsim.plots<- function(results, country, variety, useCase, Crop, produce_EXTE_plots=FALSE, yield_column=NULL){
  if (!is.null(yield_column) && (yield_column %in% names(results))){
    results$Yield <- NULL
    print(paste(yield_column, "selected as Yield."))
    results <- results %>% dplyr::rename(Yield = !!yield_column)
    }

  results <- results %>%
    filter(Variety == variety) %>%
    group_by(file_name) %>%
    filter(n() >= 5) %>%
    ungroup()
  
  unique_files <- unique(results$file_name)
  
  smdata <- results %>%
    filter(file_name == unique_files[1]) %>%
    select(SimulationID, Yield, SowDate) %>%
    arrange(SimulationID)
  
  results <- results %>%
    mutate(
      SowDate_date = as.Date(paste0(SowDate, "-2020"), format = "%d-%b-%Y"),
      SowDate_factor = factor(SowDate, levels = unique(SowDate[order(SowDate_date)]))
      
    )
  
  if (produce_EXTE_plots){  
    for (fn in unique_files) {
      p <- results %>%
        filter(file_name == fn) %>%
        ggplot(aes(x = SowDate_factor, y = Yield)) +
        geom_point(na.rm = TRUE) +
        ggtitle({
          path_parts <- str_split(fn, "/")[[1]]
          n <- length(path_parts)
          location <- path_parts[n-2]
          experiment <- path_parts[n-1]
          paste0("Yield for ", location, " (", experiment, ")")
        }) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      
      print(p)

    path_parts <- str_split(fn, "/")[[1]]
    n <- length(path_parts)
    location <- path_parts[n-2]
    experiment <- path_parts[n-1]
    
    outfile <- paste0("yield_", location, "_", experiment, ".png")
    
    outdir <- dirname(fn)
    
    parts <- str_split(outdir, "/")[[1]]
    new_parts <- parts[1:(length(parts) - 3)]
    
    outdir <- paste0(paste(new_parts, collapse = "/"), "/")
    outdir <- sub("/transform/", "/result/", outdir)
    outdir <- file.path(outdir, "EXTE")
    
    if (!dir.exists(outdir)) {
      dir.create(outdir, recursive = TRUE)
    }
    
    ggsave(
      filename = file.path(outdir, outfile),
      plot = p,
      width = 8,
      height = 5
    )
    
    }
    }
  
  glimpse(results)
  
  results <- results %>% 
    dplyr::rename(Longitude = longitude,
                  Latitude = latitude)
  
  final <- mutate(results, lonlat = paste0(Longitude, "_", Latitude))
  
  p_Win <- final  %>% 
    group_by(Longitude, Latitude)%>%
    arrange(desc(Yield)) %>% 
    slice(1:10) %>%
    as.data.frame() 
  
  pd <- final %>%
    group_by(Longitude, Latitude )%>%
    slice(which.max(Yield)) %>%
    as.data.frame() 
  
  country_ori <- country
  country <- geodata::gadm(country = country, level = 0, path = tempdir())
  country <- st_as_sf(country)
  
  p1 <- ggplot() + 
    geom_sf(data=country, fill = "white") +
    geom_point(data=pd, 
               aes(x=Longitude, y=Latitude, color= SowDate_factor), 
               size = 2) +
    labs(title = "Best Performing Sowing Date by Location",
         color = "Sowing Date")
  ggsave(paste0("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country_ori, "_", useCase, "/", Crop, "/result/APSIM/", country_ori, "_", useCase, "_", variety, "_best_sowdate_by_loc.png"),
         p1, width = 8, height = 6, dpi = 300)
  print(p1)
  
  # p2 <- ggplot() +  geom_point(data=pd, aes(x=Longitude, y=Latitude, color=SowDate_factor), size = 2)
  
  p3 <- ggplot() + 
    geom_sf(data=country, fill = "white") +
    geom_point(data=pd, 
               aes(x=Longitude, y=Latitude, color= Yield), 
               size = 2) +
    labs(title = "Highest Yield by location")
  ggsave(paste0("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country_ori, "_", useCase, "/", Crop, "/result/APSIM/", country_ori, "_", useCase, "_", variety, "_highest_yield_by_loc.png"),
         p3, width = 8, height = 6, dpi = 300)
  print(p3)
  
  # p4 <- print(ggplot() +  geom_point(data=pd, aes(x=Longitude, y=Latitude, color= Yield), size = 2)
  # return(final)
}
