##### Multi_sets.r
# 
# Brendon Fuhs
# updated 2-11-13
# 
# NON-DEFAULT PACKAGES REQUIRED
# ggplot2, (xtable?)
# 
# FUNCTIONS
#
# makeStatsTable <- function(x, categories)
#   # makes a table of descriptive stats for a single dataset split up by category
# makeFitsTable <- function(x, categories, analysis, modelNames=NULL)
# createDiffsByFactor <- (absolute times, categories)
#   # Creates frequencies from times
#
##### OLD/UNFINISHED/DELETE
# plotDensitiesByFactor?
# powerFits?
# something to create multiple diffsets and durationsets?
#

#library(xtable)
library(ggplot2)

addCategory <- function(x, tableFun, categories1, categories2, ...){
  
  dataTable <- NULL
  for (category in categories2){
    #dataTable <- cbind(dataTable, tableFun(x[categories2=category], categories1, ...))
    dataTable <- cbind(dataTable, do.call(tableFun, as.list(c( x[categories2=category], categories1, as.list(...)))))
  }
  
  return (dataTable)
}

# Use xtable on this
### Should I include a "total" or "everything" column? I probably should.
makeStatsTable <- function(x, categories){
  # maybe check that they're the same length
  categories <- as.factor(categories)
  
  factorStats <- function(category, x)
  {
    return (getStats(x[categories == category]))
  }
  statsTable <- sapply(levels(categories), factorStats, x=x)
  
  colnames(statsTable) <- levels(categories)
  #statsTable <- cbind(statsTable, "All data" = getStats(x))
  return (statsTable)
}

# can use with hazardAnalysis
# analysis is a choice of function from Stats_stuff.r
#### 2-10 added "=hazardAnalysis"
makeFitsTable <- function(x, categories, analysis=hazardAnalysis, maxLength=30, modelNames=NULL){
  ## modelNames is unused
  categories <- as.factor(categories)
  
  newCategories <- categories
  for (level in levels(categories)){ # Do I need to worry about NAs here?
    
    if (length(newCategories[newCategories==level & !is.na(x)]) < maxLength){
      x <- x[newCategories != level]
      newCategories = factor(newCategories[newCategories!=level])
      print(paste( level, " category omitted due to not enough values"))
    }
  }
  categories <- newCategories
  
  print(levels(categories))##################
  print("HEY!") #####################
  
  getFits <- function(category, x, analysis)
  {
    print(category)    ####### 
    print(length(x[categories == category & !is.na(x)] ))   #######
    print(class(x[categories == category]))    #######
    print(head(x[categories == category & !is.na(x)] ))     #######
    if (length(x[categories == category])==0){ #######
      print("GOLLEE!") #######
      return(NULL) ###########
    } ######################## ALL THE NA'S ARE AT THE END???
    return ( analysis(x[categories == category], as.character(category)) )
  }

  fitsInfoTable <- sapply(levels(categories), getFits, x=x, analysis=analysis)

  colnames(fitsInfoTable) <- levels(categories)
  #fitsInfoTable <- cbind(fitsInfoTable, "All data" = analysis(x, "all data"))
  return (fitsInfoTable)
}

# createDiffsByFactor (absolute times, categories)
# * Does not have to be sorted by times
createDiffsByFactor <- function(times, categories){
  categories <- as.factor(categories)
  names(times) <- 1:length(times) # breaks if times is length 1 or less
  
  sortVector <- diffs <- rep(NA, length(times))
  
  for (level in levels(categories)){
    if (length(categories==level) > 0 ) {
      theseTimes <- sort(times[categories==level], na.last = TRUE)
      diffs[categories==level] <- createDiffs(theseTimes)
      sortVector[categories==level] <- names(theseTimes) 
    }
  }
  #print(length(sortVector))###################### (debugging)
  #print(length(diffs))########################### yup these are the same
  #print(sortVector[is.na(sortVector)])## SHOULD THRE BE NA's in sortVector?
  
  diffs <- diffs[order(as.numeric(sortVector))] ###
  
  return (diffs)
}

###########################
### OLD OLD OLD #######
####################

makeFitsTableOLD <- function(allDataVec, categories){

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

