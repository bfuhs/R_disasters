##### Multi_sets.r
# 
# Brendon Fuhs
# updated 11-19-12
# 
# How to import Stats_stuff.r and Data_prep.r ?
# 
# INCOMPLETE makeTable <- function(allDataVec, categories)
# 
# createDiffsByFactor <- (sorted absolute times, categories)
#
# something to create multiple diffsets and durationsets?

library(ggplot2)
#library(gridExtra)


# makeStatsTable

makeFitsTable <- function(allDataVec, categories){

  if (length(allDataVec)!=length(categories)){
    print ("data and category vectors are not the same length")
    return (NULL)
  }
  
  # create list/vector of subsets using factor
  subsets <- split(allDataVec, factor(unlist(categories)))

  # function to generate info I'm looking for
  # Calls Stats_stuff.r
  analyzeSubset <- function(subset){
    if (length(subset[!is.na(subset)])<5){ #### I need to exclude 0's here too
      return (NA)
    }
    subsetInfo <- hazardAnalysis(subset, "nullname") # I should as.character(subset)
    modelFits <- subsetInfo$fitList
    modelChisqStats <- subsetInfo$chiSquareList
    pars <- list()
    chis <- list()
    for (dist in modelFits){
      pars <- c(pars, list(dist$estimate))
    }
    for (chisqStat in modelChisqStats){
      chis <- c(chis, chisqStat)
      # chis <- c(chis, list(chisqStat$statistic)) # or maybe I should get p-value?
    }
    names(chis) <- names(pars) <- names(modelFits)
    N <- length(subset)
    statstcs <- getStats(subset)
    return (list("N"=N, "parameters"=pars, "chiSquareStats"=chis, "stats"=statstcs ) )
  }
  
  # Create matrix of info by applying above function
  subsetsInfo <- sapply(subsets, analyzeSubset)
  names(subsetsInfo) <- names(subsets)

  # Show the table
  View(subsetsInfo) ###### This is the second worst way to do it
  
  return (subsetsInfo)
}

# createDiffsByFactor <- (sorted times, categories)
#### still need to use sorted times
createDiffsByFactor <- function(times, categories){
  diffs <- rep(NA, length(times))
  
  for (level in levels(factor)){
    theseTimes <- times[categories==level]
    diffs[categories==level] <- createDiffs(theseTimes)
  }
  
  return (diffs)
}

## I don't know about this
plotDensitiesByFactor <- function(x, categories){
  categories <- as.factor(categories)
  ##thisPlot <- ggplot()
  ##for (level in levels(categories)){
  ##  thisPlot <- thisPlot + geom_density(x[categories==level])
  ##  
  ##}
  #df <- data.frame(cat=categories, dat=x)
  #plt <- ggplot(df, aes(x=dat, fill=cat)) + geom_density(alpha=.3) 
  #plt <- plt + scale_y_log10(limits = c(1,20)) + scale_x_log10(limits = c(1,100))

  #plt
  for (level in levels(categories)){
    plotEmpirical(x[categories==level], level)
  }  
}

powerFits <- function(x, categories){
  for (level in levels(categories)){
    fitPower(x[categories==level], level)
  }
}

