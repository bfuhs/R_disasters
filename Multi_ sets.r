# 
# Multi_sets.r
# 
# Brendon Fuhs
# updated 11-11-12
# 
# How to import Stats_stuff.r and Data_prep.r ?
# 
# 

#library(ggplot2)
#library(gridExtra)


makeTable <- function(allDataVec, categories){

  if (length(allDataVec)!=length(categories)){
    print ("data and category vectors are not the same length")
    return (NULL)
  }
  
  # create list/vector of subsets using factor
  subsets <- split(allDataVec, factor(unlist(categories)))

  # function to generate info I'm looking for
  # Calls Stats_stuff.r
  analyzeSubset <- function(subset){
    if (length(subset[!is.na(subset)])<5){
      return (NA)
    }
    subsetInfo <- hazardAnalysis(subset, "nullname") # I should as.character(subset)
    modelFits <- subsetInfo$fitList
    pars <- list()
    for (dist in modelFits){
      pars <- c(pars, list(dist$estimate))
    }
    names(pars) <- names(modelFits)
    return (pars)
  }
  
  # Create matrix of info by applying above function
  subsetsInfo <- sapply(subsets, analyzeSubset)
  names(subsetsInfo) <- names(subsets)

  # Show the table
  View(subsetsInfo) ###### This is the second worst way to do it
  
  return (subsetsInfo)
}