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

#### Getting Samples:
# disasterData[sample(1:length(disasterData[,1]), 30),]
# disasterSample <- bigDisasters[sample(1:length(bigDisasters[,1]), 30),]
# OFDAsample <- OFDAdata[sample(1:length(OFDAdata[,1]), 30),]
# sort(OFDAsample[[1]])
#
# Can make the smoothing widen with magnitude in these. Yes
#
# cost<-na.omit(bigDisasters[,10])
# costPDF = density(cost)
# plot(costPDF, log="xy", ylim=c(1e-08,1e-01))
#
# affected<-na.omit(bigDisasters[,9])
# affectedPDF = density(affected)
# plot(affectedPDF, log="xy", ylim=c(1e-11,1e-03))
# 
# deaths<-na.omit(bigDisasters[,8])
# deathsPDF = density(deaths)
# plot(deathsPDF, log="xy", ylim=c(1e-10,1e-02))
#
# now to log(x), log(1-ecdf(x))
#
# plot(log(cost), log(1-ecdf(cost)(cost)))
# plot(log(affected), log(1-ecdf(affected)(affected)))
# plot(log(deaths), log(1-ecdf(deaths)(deaths)))
#
# startDates<-readClipboard()
# sapply(startDates, as.Date, format = "%m/%d/%Y")
# diffDates<-startDates[-1] - startDates[-length(startDates)]
# (more than 10000 are zeroes)
# (more than 5000 are ones)
# modernStartDates<-startDates[3491:length(startDates)]
# modernDiffDates<-modernStartDates[-1] - modernStartDates[-length(modernStartDates)]

######### OLD STUFF

inputData <- function(){
  print("Your working directory is...")
  print(getwd())
  
  # (IRL prompt for the filename string)
  # Import from csv and stringify everything
  disasterData<-read.csv("OFDAcountryResponse.csv",
                         header = TRUE,
                         sep = "\t",
                         colClasses= "character") 
  # Maybe should have used "as.is=T" instead of colClasses?
  
  print (names(disasterData))
  
  # Trim  off beginning and ending whitespace
  disasterData <- sapply(disasterData, str_trim)
  
  # disasterData[,7] is the dates. I can't replace it in the data frame,
  # so I create a new list of lists
  declarationDates <- str_split(disasterData[,7], ",")
  declarationDates <- sapply(declarationDates, str_trim)
  
  # Change to Date format
  declarationDates <- sapply(declarationDates, as.Date, format = "%m/%d/%Y")
  
  # sort each list
  declarationDates <- sapply(declarationDates, sort)
  
  declarationDates <- sapply(declarationDates, unlist)
  
  declarationDates<-sapply(declarationDates, unlist)
  
  
  # get country codes, maybe sapply is better?
  countryCodes = lapply(disasterData[,1], substr, start=1, stop=3)
  
  startFY <- as.numeric(disasterData[,2])
  endFY <- as.numeric(disasterData[,3])
  cost <- as.numeric(disasterData[,10]) # gives warning about NA's
  regions = disasterData[,4]
  
  # not sure if I should be making these factors or not
  regions <- as.factor(regions)
  startFY <- as.factor(startFY)
  endFY <- as.factor(endFY)
  countryCodes <- as.factor(unlist(countryCodes))
  
  startingDeclarationDates <- sapply(declarationDates, head, n=1)
  endingDeclarationDates <- sapply(declarationDates, tail, n=1)
  # I need to exclude FY 93 and 2010 from some of these
  # diffDays <- unlist(endingDeclarationDates) - unlist(startingDeclarationDates)
  # also this isn't long enough
  affected <- disasterData[,8]
  dead <- disasterData[,9]
  # don't forget cost is a response variable too!
  
  # regionCostTable <- table(regions, cut(cost, breaks = 10^(3:10)))
  # countryCostTable <- table(countryCodes, cut(cost, breaks = 10^(3:10)))
  # barplot(regionCostTable)
  # barplot(countryCostTable)
  
  # better:
  costRegionTable = table(cut(cost, breaks = 10^(3:10)), regions)
  barplot(costRegionTable)
  costCountyTable <- table( cut(cost, breaks = 10^(3:10)), countryCodes)
  barplot(costCountyTable, horiz=TRUE)
  # looks too cluttered with or without horiz
  
  ### Okay now I'm going to do it getting rid of FY 93 and FY 10
  
  excludeThese = disasterData[,2] =="1993" |  disasterData[,2] =="2010" |  disasterData[,3] =="1993" | disasterData[,3] =="2010"
  constrictedDisasterData = disasterData[!excludeThese,]
  constrictedDeclarationDates <- str_split(constrictedDisasterData[,7], ",")
  constrictedDeclarationDates <- sapply(constrictedDeclarationDates, str_trim)
  constrictedDeclarationDates <- sapply(constrictedDeclarationDates, as.Date, format = "%m/%d/%Y")
  constrictedDeclarationDates<- sapply(constrictedDeclarationDates, sort)
  constrictedDeclarationDates<- sapply(constrictedDeclarationDates, unlist)
  
  # get country codes, maybe sapply is better?
  constrictedCountryCodes = lapply(constrictedDisasterData[,1], substr, start=1, stop=3)
  
  constrictedStartFY <- as.numeric(constrictedDisasterData[,2])
  constrictedEndFY <- as.numeric(constrictedDisasterData[,3])
  constrictedCost <- as.numeric(constrictedDisasterData[,10]) # gives warning about NA's
  constrictedRegions = constrictedDisasterData[,4]
  
  # not sure if I should be making these factors or not
  constrictedRegions <- as.factor(constrictedRegions)
  constrictedStartFY <- as.factor(constrictedStartFY)
  constrictedEndFY <- as.factor(constrictedEndFY)
  constrictedCountryCodes <- as.factor(unlist(constrictedCountryCodes))
  
  constrictedStartingDeclarationDates <- sapply(constrictedDeclarationDates, head, n=1)
  constrictedEndingDeclarationDates <- sapply(constrictedDeclarationDates, tail, n=1)
  
  constrictedAffected <- constrictedDisasterData[,8]
  constrictedDead <- constrictedDisasterData[,9]
  
  excludeFromDiffDays <- lapply(constrictedStartingDeclarationDates, length) == 0 | lapply(constrictedEndingDeclarationDates, length) == 0
  diffDays <- unlist( constrictedEndingDeclarationDates[!excludeFromDiffDays] ) - unlist( constrictedStartingDeclarationDates[!excludeFromDiffDays] )
  diffDayCost <- constrictedCost[!excludeFromDiffDays]
  
  # plot(diffDays,diffDayCost, log="xy")
  # plot(diffDays+183,diffDayCost, log="xy")
  
  diffDaysNoZero <- diffDays[diffDays!=0]
  diffDaysNoZeroCost <- diffDayCost[diffDays!=0]
  
  
  adjustedDiffDaysNoZero = diffDaysNoZero + 183
  
  logFit<-lm(log(diffDaysNoZeroCost[diffDaysNoZeroCost!=0])
             ~log(adjustedDiffDaysNoZero[diffDaysNoZeroCost!=0]))
  plot(log(diffDaysNoZero),log(diffDayNoZeroCost),
       xlab="log Durations", ylab="log Costs")
  abline(logFit, col="red")
  
  summary(logFit)
  
  title(main="p value = 4.867e-12, or so the summary function tells me")
  
  # This is not a duration as above
  # gotta unlist stuff
  DeclDatesNoDuration <- unlist(constrictedEndingDeclarationDates[unlist(constrictedEndingDeclarationDates) == unlist(constrictedStartingDeclarationDates)])
  seqntlDeclDatesNoDuration <- sort(DeclDatesNoDuration)
  intervalDaysNoDuration <- seqntlDeclDatesNoDuration[-1] - seqntlDeclDatesNoDuration[-length(seqntlDeclDatesNoDuration)]
  intervalDaysNoDurationNoZero <- intervalDaysNoDuration[intervalDaysNoDuration!=0]
  # I realy don' likehow I'm preparing this
  # It doesn't seem like I'm capturing anything meaningful here.
  
  print(Hypothesis1(intervalDaysNoDurationNoZero))
  
  
  # return stuff
}