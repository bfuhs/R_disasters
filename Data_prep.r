##### Data_prep.r
#
# Brendon Fuhs
# Updated 2-11-13
# 
# This module contains functions to help munge and parse disaster data.
#
# NON-DEFAULT PACKAGES REQUIRED
# stringr, quantmod
#
# FUNCTIONS
# getRealUSD <- function (times, nomUSD)
# createDurations <- function(startTimes, endTimes)
# createDiffs <- function(times)
# composeMagnitudes <- function(magnitudes)
# parseData <- function(lots of stuff) # NOTE QUITE WORKING
#
# USAGE
# Make sure that csv files are in the working directory! ( getwd() )
# Enter a command like these:
#  disasterData <- read.csv(filenameString, sep = "\t", stringsAsFactors=FALSE ) # , colClasses= "character")
#  disasterData <- read.table("clipboard", ,sep="\t")
# String data should be not be imported as factors ( stringsAsFactors=FALSE )
# etc.
#

library(stringr)
library(quantmod) ## for inflation adjusting


# http://stackoverflow.com/questions/12590180/inflation-adjusted-prices-package
### ONLY GOES BACK TO 1947 ### ALSO THIS IS APPARENTLY BROKEN
getRealUSD <- function (times, nomUSD){
  getSymbols("CPIAUCSL", src='FRED')
  # Stores CPI as CPIAUCSL
  
  deflate <- function(cpi, nomPrice){
    nowCpi <- tail(cpi,1)
    realPrice <- nomPrice * nowCpi / cpi
    return(realPrice)
  }
  
  ## VVV DOES THIS BREAK IF TIME TOO LATE? Also, this is inefficient
  roundUpTimes <- function(time, availableTimes){
    return (min(availableTimes[availableTimes > time]))
  }
  roundedTimes <- sapply(times, roundUpTimes, availableTimes=names(CPIAUCSL))
  CPIs <- CPIAUCSL[roundedTimes] ## Will this indexing technique work?
  
  realUSD <- mapply(deflate, cpi=CPIs, nomPrice=nomUSD)
  return (realUSD)
}

createDurations <- function(startTimes, endTimes){
  startTimes <- as.numeric(startTimes)
  endTimes <- as.numeric(endTimes)
  if (length(startTimes)!=length(endTimes)){
    print ("vectors are not the same length")
    return (NULL)
  }
  durations <- endTimes - startTimes
  
  return (durations)
}

# This one requires sorted times
createDiffs <- function(times){
  diffs <- createDurations( times[-length(times)], times[-1] )
  return (c(diffs, NA))
}


### PLACEHOLDER
composeMagnitudes <- function(magnitudes){
  return (magnitudes)
}

### Wrapper to do all of the above based on simple inputs
### Feed it vectors of columns numbers (or names?)
parseData <- function(rawData,
                      factorCols=c(), magnitudes=c(), dates=c(), 
                      nominalUSD=c(), USDtimes=c(),
                      datesToDiff=c(), startDates=c(), endDates=c()){
  dataNames <- names(rawData)
  rawData <- sapply(rawData, str_trim)
  rawData <- data.frame(rawData, stringsAsFactors = FALSE)
  names(rawData) <- dataNames
  
  # May need to do more specific munging
  
  ### Do I need to talk about NAs?
  rawData[,factorCols] <- sapply(rawData[,factorCols], as.factor) #### Not working?
  rawData[,magnitudes] <- sapply(rawData[,magnitudes], as.numeric)

  for (date in dates){
    if (str_detect(rawData[,date], "/")==TRUE){ 
      rawData[,date] <- as.Date(rawData[,date], format = "%m/%d/%Y", origin="1970-01-01") # or day before?
    } else if (str_detect(rawData[,date], "-")==TRUE) {
      rawData[,date] <- as.Date(rawData[,date], format = "%m-%d-%Y", origin="1970-01-01")
    } else {
      print ("MADE IT")
      print(dates)###
      print(date)###
      print(head(rawData[,date]))###
      rawData[,date] <- as.Date(as.numeric(rawData[,date]), origin="1899-12-31")
      #rawData[,date] <- sapply(rawData[,date], as.Date, origin="1899-12-31")
    }
  }
  #for (USDcol in nominalUSD){
  #  rawData[ paste("real", names(rawData)[USDcol]) ] <- getRealUSD(rawData[,USDtimes], rawData[,USDcol])     
  #}
  
  # Need Multi_sets loaded
  for (date in datesToDiff){
    rawData[paste(names(rawData)[date], "differences")] <- createDiffsByFactor(rawData[,date], rep(1,length(rawData[,date])))
  }
  
  ## createDurations
  ## Do stuff by factor
  ## composeMagnitudes(magnitudes)

  return(rawData)
}

