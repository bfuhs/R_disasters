##### Data_prep.r
#
# Brendon Fuhs
# Updated 11-19-12
#
# Functions to use:
#
# importCSV(filename)         ### import data from csv file
# importClipboard()           ### import data from clipboard
# importOFDA(filename)        ### import data from OFDA csv file
# importCRED(filename)        ### import data from CRED/EMDAT csv file
# sortByCol(data frame, rowName) ### sort a dataframe according to a particular column
#   ^^^BROKEN
# createDiffs(times)          ### create a vector of time-differences from times
# createDurations(startTimes,endTimes) ### create vector of durations from vectors of start and end times
#   ^^^BROKEN
# composeMagnitudes(other magnitude vectors) ### Will create a composite of magnitudes
#   ^^^PLACEHOLDER
#
######
#
# Make sure that csv files are in the working directory! ( getwd() )
# Note that throughout, it is assumed that csv files
# are delineated by TABs, not commas, and have column titles
#
######
#
# Pack requirement: stringr

library(stringr)


importCSV <- function(filenameString){
  disasterData <- read.csv(filenameString, sep = "\t" ) # , colClasses= "character")
  return (disasterData)
}

importClipboard <- function(){
  disasterData <- read.table("clipboard", ,sep="\t")
  return (disasterData)
}

importOFDA <- function(filename){
  OFDAdata <- importCSV(filename)
  OFDAdata <- sapply(OFDAdata, str_trim)
  
  countryCode = lapply(OFDAdata[,1], substr, start=1, stop=3)
  country <- OFDAdata[,5]
  region <- OFDAdata[,4]
  startFY <- as.numeric(OFDAdata[,2])
  endFY <- as.numeric(OFDAdata[,3])
  affected <- as.numeric(OFDAdata[,8])
  dead <- as.numeric(OFDAdata[,9])
  cost <- as.numeric(OFDAdata[,10])

  declDates <- str_split(OFDAdata[,7], ",")
  declDates <- sapply(declDates, str_trim)
  declDates <- sapply(declDates, as.Date, format = "%m/%d/%Y")
  declDates <- sapply(declDates, sort)
  # declDates <- sapply(declDates, unlist)
  
  startDeclDate <- sapply(declDates, head, n=1)
  startDeclDate[startFY==1993] <- NA
  endDeclDate <- sapply(declDates, tail, n=1)
  endDeclDate[endFY==2010] <- NA
  
  OFDAdata <- as.data.frame( cbind( countryCode,
                                    country,
                                    region,
                                    startFY,
                                    endFY,
                                    startDeclDate,
                                    endDeclDate,
                                    affected,
                                    dead,
                                    cost ),
                             row.names = c( "countryCode",
                                            "country",
                                            "region",
                                            "startFY",
                                            "endFY",
                                            "startDeclDate",
                                            "endDeclDate",
                                            "affected",
                                            "dead",
                                            "cost" ) )
  #reorder<- function(dframe,ranking){
  #  outframe <- data.frame("deleteMe"=rep(NA, nrow(dframe)))
  #  for (name in names(dframe)){
  #    outframe[name] <- cbind(outframe, name=dframe$name[ranking])
  #  }
  #  return (outframe)
  #}
  
  #endOrder <- rank(as.numeric(endDeclDate), ties.method="first")
  #OFDAdata <- reorder(OFDAdata,endOrder)
  #endDiffDays <- as.numeric(endDeclDate[-1]) - as.numeric(endDeclDate[-length(endDeclDate)])
  #OFDAdata$endDiffDays <- c(endDiffDays, NA)
  
  #startOrder <- rank(as.numeric(startDeclDate), ties.method="first")
  #OFDAdata <- reorder(OFDAdata,startOrder)
  #startDiffDays <- as.numeric(startDeclDate[-1]) - as.numeric(startDeclDate[-length(startDeclDate)])
  #OFDAdata$startDiffDays <- c(startDiffDays, NA)
  
  return (OFDAdata)
}

## Not really functional
importCRED <- function(filename){
  CREDdata <- importCSV(filename)
  
  ### These are commands used 11-19-12
  # bigDisasters <- importCRED("disaster-11-19-12.csv")
  # names(bigDisasters)
  # startDates <- as.Date(bigDisasters[,1], format = "%d/%m/%Y")
  # length(startDates)
  # [1] 19976
  # length(startDates[!is.na(startDates)])
  # [1] 16277
  # endDates <- as.Date(bigDisasters[,2], format = "%d/%m/%Y")
  # length(endDates)
  # [1] 19976
  # length(endDates[!is.na(endDates)])
  # [1] 16313
  # qplot(startDates)
  # orderedDisasters <- bigDisasters[ order( as.numeric(startDates) ), ]
  # startDates <- as.Date(bigDisasters[,1], format = "%d/%m/%Y")
  # endDates <- as.Date(bigDisasters[,2], format = "%d/%m/%Y")
  # durations <- createDurations(startDates, endDates)
  # length(durations[!is.na(durations)])
  # [1] 16194
  # length(durations[durations!=0])
  # [1] 7511
  # 7511 - (19976-16194)
  # [1] 3729
  # qplot(durations[durations!=0])
  # nonZeroDurations<-durations[durations!=0]
  # nonZeroDurations<-nonZeroDurations[!is.na(nonZeroDurations)]
  # nonZeroDurations[nonZeroDurations <= 0]
  # [1] -16 -25  -1
  # stuff <- analyzeDurations(nonZeroDurations, "durations")
  # plotDensitiesByFactor(durations, bigDisasters[,7])
  # fitPower(durations,"durations")
  # powerFits(durations, bigDisasters[,7])
  # qplot(nonZeroDurations, binwidth=1)
  # analyzeDurations(durations,"durations")
  
  ### These are commands used for one time
  #Eafrica <- importCSV("EafricaCondensed.csv")
  ### quick and dirty change 00 to 01
  #EafricaDates <- sapply(Eafrica[[5]], str_replace_all, pattern="00/", replacement="01/")
  #EafricaDates <- as.Date(EafricaDates, format = "%d/%m/%Y")
  #qplot(EafricaDates)
  #sortedEafricaDates <- sort(EafricaDates)
  #EafricaDiffsSince1970 <- createDiffs(sortedEafricaDates[8:410])
  #EafricaDiffsSince1993 <- createDiffs(sortedEafricaDates[64:410])
  #hazardAnalysis(EafricaDiffsSince1993, "since 1993")
  #hazardAnalysis(EafricaDiffsSince1970, "since 1970")
  #sortedEafrica <- Eafrica[ order( as.numeric(Eafrica$Start..) ), ]
  #ughstuff<-makeTable(EafricaDiffsSince1970, Eafrica$Type[8:410])
  #ughstuff<-makeTable(EafricaDiffsSince1993, Eafrica$Type[64:410])
  
  return (CREDdata)
}

#sortByRow <- function(dFrame, colName){
#  print (as.numeric(dFrame[[colName]]))
#  print (class(as.numeric(dFrame[[colName]])))
#  #return ( dFrame[ order(as.numeric(dFrame$colName)), ] )
#  return ( dFrame[ order(dFrame[[colName]]), ] )
#  
#}
################### > testtt <- testOFDA[ order( as.numeric(testOFDA$startDeclDate) ), ]
### dFrameCol needs to be dFrame$colName


sortByCol <- function(dFrame, dFrameCol){
  return (dFrame[ order(as.numeric(dFrameCol)) ])
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

createDiffs <- function(times){
  diffs <- createDurations( times[-1], times[-length(times)] )
  return (c(diffs, NA))
}

### PLACEHOLDER
composeMagnitudes <- function(magnitudes){
  return (magnitudes)
}
