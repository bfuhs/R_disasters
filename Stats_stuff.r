# Stats_stuff.r
#
# file created 9-19-12 by Brendon Fuhs
# last updated 9-24-12
#
### Uses the following packages:
#
### Usage:
# large samples, continuous models/domains
#
### Models list
#
### Issues
#


library(stringr)
library(moments)
# Do I want bbmle?
library(bbmle)

## function to Handle data
# takes in data, cleans/operates on it, spits out data
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


LogLExp <- function(a, obs){
  n<-length(obs)
  LogL=n*log(a) - a*sum(obs)
  if (is.na(LogL)) 
    LogL <- -1e+100
  if (LogL <= -1e+100)
    LogL <- -1e+100
  return (-LogL)
}

#LogLWeib <- function(theta, obs){
#  a=theta[1]
#  b=theta[2]
#  n<-length(obs)
#  LogL <- n*log(a*(b^-a)) + (a-1)*sum(log(obs)) - sum(obs^a)/(b^a)
#  return (-LogL)
#}

LogLWeib <- function(a,b,obs){
#  a = theta[1]
#  b = theta[2]
  n <- length(obs)
  LogL <- n*log(a/(b^a)) + (a-1)*sum(log(obs)) - sum(obs^a)/(b^a)
  #LogL <- n*log(a/b) + (a-1)*sum(log(obs/b)) - sum(obs^a)/(b^a)
  if (is.na(LogL)) 
    LogL <- -1e+100
  if (LogL <= -1e+100) 
    LogL <- -1e+100
  return (-LogL)
}

pValFromLRT <- function(LogL0, LogL1, paramNumDiff){
  Dstat <- 2*LogL0 - 2*LogL1
  
  # We want to return the prob that chisq spits out something
  # more extreme than this
  return (1 - pchisq(Dstat, paramNumDiff))
}

empCDF<-ecdf(obs)
#can use plot.ecdf directly on this
###
# NEED TO FIX/CHECK THIS SHIT RIGHT HERE!!!!
###
#
# empPDF use kernel estimation/smoothing?
# KernSmooth package?
# Or use "density"
# Then what?
#
# See obsolete DisasterTest2.r for comments
empPDF<-function(obs){ 
  obsvals=unique(obs)
  xpts=rowMeans(cbind(obsvals[-1], obsvals[-length(obsvals)]))
  rise=empCDF(obsvals)[-1] - empCDF(obsvals)[-length(obsvals)] ### relaced empCDF with ecdf
  run=obsvals[-1]-obsvals[-length(obsvals)]
  ypts=rise/run
  return (approxfun(xpts, y=ypts, yleft=0, yright=0))
}

# Shapiro-Wilk test for log-normality
Hypothesis0 <- function(diffs){
  
  shapiro.test(diffs)
  
  shapiro.test(log(diffs)) ## Is this how to do the log-normality?
  
}

# Exp vs Weib
Hypothesis1 <- function(diffs){
  
  meanDiff <- mean(diffs)
  
  #expFit <- optim( 1/(meanDiff+1), LogLExp, obs=diffs,
  #                 method = "SANN")
                  # method = "L-BFGS-B" , lower = 1e-4)
  
  #weibFit <- optim(c(10, meanDiff), LogLWeib, obs=diffs,
  #                 method = "SANN")
                   # method = "L-BFGS-B", lower = c(1e-4,1e-4))
  
  expFit <- mle2(LogLExp, start=list(a=1/(meanDiff+3) ),
                 #method="SANN", data=list(obs=diffs),
                 #optimizer="optim")
                 method="L-BFGS-B", data=list(obs=diffs),
                 optimizer="optim", lower=1e-6)
  
  weibFit <- mle2(LogLWeib, start=list(a=1, b=meanDiff-3),
                  #method="SANN",data=list(obs=diffs),
                  #optimizer="optim")
                  method="L-BFGS-B",data=list(obs=diffs),
                  optimizer="optim", lower=c(1e-6,1e-6))
  
  #print (summary(expFit))
  #print (summary(weibFit))

  ####### HOW DO I EXTRACT THE LIKELIHOOD? #############
  #print(pValFromLRT(logLik(expFit), logLik(weibFit), 1))

  # xx = c(1:145)
  # plot(xx, ecdf(diffs)(xx))
  # plot(xx, empPDF(diffs)(xx))
  
  hist(diffs, freq=FALSE, breaks = seq(0,150,5))
  lines(density(diffs), col = "red")
  curve(dexp(x,coef(expFit)), col="blue", add=TRUE)
  curve(dweibull(x,coef(weibFit)[1],coef(weibFit)[2]), col="green", add=TRUE)
}

# Divide cost by a power of ten so it doesn't overwhelm things
Hypothesis2 <- function(diffs){
  
  # First, OLM on logs
  diffs=diffs[diffs!=0 & !is.na(diffs)]
  empCDF = ecdf(diffs)
  diffs = diffs[diffs!=max(diffs)] # Get rid of max to keep logs from breaking
  Xlogs = log(diffs)
  Ylogs = log(1 - empCDF(diffs))
  
  loggyFit <- lm(Ylogs ~ Xlogs)
  print(summary(loggyFit))
  
  plot(Xlogs,Ylogs)
  abline(loggyFit, col="blue")

  
}

# Move this to the top
 # for skew and kurtosis
printStats <- function(x){
  # Vital stats of vector x
  print("Mean = ")
  print(mean(x, na.rm=TRUE)) #, took out na.rm=TRUE - didn't make a difference
  print("Variance = ")
  print(var(x, na.rm=TRUE))
  print("Skewness = ")
  print(skewness(x, na.rm=TRUE))
  print("Kurtosis = ")
  print(kurtosis(x, na.rm=TRUE))
  print("Std.dev. = ")
  print(sd(x, na.rm=TRUE))
  print("median = ")
  print(median(x, na.rm=TRUE))
  print("max = ")
  print(max(x, na.rm=TRUE))
  print("min = ")
  print(min(x, na.rm=TRUE))
  print("most frequent observation (bottom value is no. of instances) = ")
  print(sort(table(x))[length(table(x))])
  print("number of observations = ")
  print(length(x[!is.na(x)]))
  print(" ")
}


# http://answers.oreilly.com/topic/2631-how-to-create-a-bar-chart-with-r/
# RegionTotals = tapply(cost[!is.na(cost)],OFDAdata$Region[!is.na(cost)],sum)
# barplot(RegionTotals)
# I don't know why this is ugly

# countryCode=lapply(levels(disasterData[,1]),substring,1,3)
# countryCode<-unlist(countryCode)
# countryTotals = tapply(disasterData[!is.na(disasterData[,10]),10],countryCode[!is.na(disasterData[,10])],sum)
# barplot(sort(countryTotals))

# ResponsesPerCountry <- tapply(levels(disasterData[,1]),countryCode,length)
# barplot(sort(ResponsesPerCountry))

# ResponsesPerRegion <- tapply(levels(disasterData[,1]),disasterData[,4],length)
# barplot(sort(ResponsesPerRegion))

## Nonparametric stuff


### Should I use an existing wrapper?
#CreateModel <- function(PDF, CDF, parNum, domainRange, LogLikFn, InitValFn){ 
#  return list("PDF"=PDF, "CDF"=CDF, "parNum"=parNum, "Domain"=domainRange, 
#              "LogLikFn"=LogLikFn,"InitValFn"=InitValFn)
#} # Note no Haz fn here
# domainRange should be a vector
### This is just something that names stuff in a list!


### Should I use an existing wrapper?
#CreateFit <- function(Model, obs){
#  # From the ModelList, we are using
#  # PDF(?), parNum, domainRange, LogLikFn, and InitValFn
#  
#  OptimizedStuff = (Model$InitValFn(obs), Model$LogLikFn, 
#                    Y=Model$obs[,1], X=Model$obs[,(2,:)],
#                    method="L-BFGS-B", lower=Model$domainRange, upper=Model$domainRange )
#  # Need to make sure obs can be multi or single - D
#  # can use "ifelse" and "NULL" and ...perhaps
#  
#  paramvals <- OptimizedStuff$par
#  LogLik <- OptimizedStuff$value
#  
#  # Kolmogorov-Smirnoff test # ?Other tests (Anderson-Darling, Kuiper, Cramer-von-Mises)
#  # BUT WHAT ABOUT MULTDIMENSIONALITY goodness-of-fit? Joint ecdf?
#  empCDF <- ecdf(obs) 
  # x=(fine range specified by domain)
  # D = max(abs(empCDF(X)-CDF(x)))
  # could optim this instead
  # http://www.mathwave.com/articles/goodness_of_fit.html
  # Compare to table to get p-value
  
  # Actually, what I really need is a way to do a comparison
  # AMONG the models I have AND the possibility that they are wrong
  # to get real legit p-values
  
  # AICinfo
#  n = length(obs[,1]) # NEED to make sure this works on multi and single dim
#  k = Model$parNum
#  AIC = 2*k + 2*LogLik
#  AICc = AIC+2*k*(k+1)/(n-k-1)
#  
#  return list("pars"=paramvals, "LogLik"=LogLik, "p_val"=pval, 
#              "AIC"=AIC, "AICc"=AICc) # , "BIC"=BICval
#}


#display <= function(things){
  # based on user specification somehow
  
#}




# Instantiate models in a list
# Provide user with description of the list

# User can execute data function
# User can decide which models to test in list.
# User decides which information to display
