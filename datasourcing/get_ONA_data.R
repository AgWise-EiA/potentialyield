
#Function to list ODK form details and find formids on ONA server
findONAdatasets <- function(user, pw){
  
  require(httr)
  
  url <- paste0("https://api.ona.io/api/v1/data?owner", user) 
  
  f <- GET(url, httr::authenticate(user, pw))
  r <- content(f)
  
  if(length(r) > 0){
    r <- do.call(rbind, lapply(r, data.frame))
  }
  
  return(r)
  
}



#Function to get raw API output data from ONA server for formid
getONAdata <- function(user, pw, id){
  
  require(httr)
  
  url <- paste0("https://api.ona.io/api/v1/data/", id)
    
  f <- GET(url, httr::authenticate(user, pw))
  r <- content(f)
  
  return(r)
  
}



#Function to decompose raw ONA output into list of dataframes based on hierarchy of nested/consecutive repeat loops
decomposeONAdata <- function(r){
  
  require(dplyr)
  require(tidyr)
  require(rrapply)
  require(stringr)
  
  ml <- list()

  if(length(r) > 0){
    #prune and melt into a dataframe with structure of nested / consecutive for loops
    mr <- rrapply::rrapply(r, how = "melt") 
    nL <- ncol(mr)-2 #extract number of levels in the hierarchy
    
    for(i in 1:nL){
      
      if(i != nL){
        mi <- mr[!is.na(mr[,paste0("L", i+1)]) & is.na(mr[,paste0("L", i+2)]),]
      }else{
        mi <- mr[!is.na(mr[,paste0("L", i+1)]),]
      }
      
      if(nrow(mi) > 0){
        if(is.list(mi$value)) {
          mi$value[unlist(lapply(mi$value, is.null))] <- NA
          mi$value <- unlist(mi$value)
        }
        mi <- mi %>%
          dplyr::select_if(~sum(!is.na(.)) > 0) %>%
          tidyr::spread(paste0("L", i+1), value) %>%
          #dplyr::rename_all(basename) #drop group names 
          dplyr::rename_all(stringr::str_extract, "\\w+$")
        ml <- append(ml, list(mi))
      }
    }  
  }
  
  return(ml)
  
}