#
# Data_prep.r
#
# Brendon Fuhs
# Updated 11-4-12
#
# Functions to use:
#
#### General data importing
# importCSV(filename) 
# importClipboard()
#
#### importing data from OFDA csv file
# importOFDA(filename)
#
#### importing data from CRED/EMDAT csv file
# importCRED(filename)
#
#### sort a data frame according to a particular row
# sortByRow(data frame, rowName)
#
#### creating a vector of time-differences from times
# createDiffs(times)
#
### Make sure that csv files are in the working directory! ( getwd() )
#
### Note that throughout, it is assumed that csv files
### are delineated by TABs, not commas, and have column titles


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

importCRED <- function(filename){
  CREDdata <- importCSV(filename)
  
  
  
  
  
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






sortByRow <- function(dFrame, dFrameCol){
  return (dFrame[ order(as.numeric(dFrameCol)) ])
}

createDiffs <- function(times){
  diffs <- as.numeric(times[-1]) - as.numeric(times[-length(times)])
  return (c(diffs, NA))
}

