##### Stats_stuff.r
# Brendon Fuhs
# updated 11-5-12
#
### Descriptive Statistics
# getStats(data)
#
# Need shapiro test? Anderson-Darling Test?
### Stationarity Testing
#   ######################### NEED THIS
#
### Plot nonparametric estimate of distribution
# plotEmpirical(data, data label)
#
### Test models
# fitModels(data, vector of distribution names, data label)
#
### Weibull Test
# hazardAnalysis(data, data label)
#
### Power Law Analysis
# powerLawAnalysis(data, data label)
# 

###################### Another way of doing power law?, LRT????


#############
# Note that these currently are only working for univariate data
# default libraries used include stats
# non-default libraries used include...
# packages that need to be downloaded include moments

# library(stringr)
library(moments)
# library(bbmle)
library(plyr)
library(MASS)

# descriptive stats of vector x
# (requires moments package)
getStats <- function(x){
  x<-as.numeric(x)
  xStats <- list( mean(x, na.rm=TRUE), #, took out na.rm=TRUE - didn't make a difference
                  var(x, na.rm=TRUE),
				          skewness(x, na.rm=TRUE),
				          kurtosis(x, na.rm=TRUE),
				          sd(x, na.rm=TRUE),
				          median(x, na.rm=TRUE),
				          max(x, na.rm=TRUE),
				          min(x, na.rm=TRUE),
				          sort(table(x))[length(table(x))],
				          length(x[!is.na(x)])
				         )
  names(xStats) <- c( "mean",
                      "variance",
					            "skewness",
					            "kurtosis",
					            "std. dev.",
					            "median",
					            "maximum",
					            "minimum",
					            "most freq. obs.",
					            "number of obs."
					          )
  return (xStats)
}

# non-parametric estimation of distribution
plotEmpirical <- function(x, label){
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  # CDF plot
  empCDF <- ecdf(x) # a function
  plot(empCDF, main = paste(label, "empirical CDF estimation"), xlab=label, ylab=paste("Cumulative probability of",label))
  
  # PDF plot
  empPDF <- density(x) # a density object
  plot(empPDF, log="y", main = paste(label, "empirical PDF estimation"), xlab=label, ylab=paste("Probability density of",label))
  
  # hazard function plot
  plot ( empPDF$x, empPDF$y/(1-empCDF(empPDF$x)), log="y", main = paste(label, "empirical Hazard function estimation"), xlab=label, ylab=paste("Hazard function of",label), type="l")
}


fitModels <- function(obs, modelNames, label){
  obs <- as.numeric(obs)
  obs <- obs[!is.na(obs)]
  obs <- obs[obs!=0] #### maybe not
  
  PDFs <- list( "beta" = dbeta,
                #"cauchy" = ,
				        #"chi-squared" = ,
				        "exponential" = dexp, 
				        #"f" = , 
				        "gamma" = dgamma,
				        "geometric" = dgeom, 
				        "log-normal" = dlnorm, 
				        "lognormal" = dlnorm, 
				        #"logistic" = , 
				        #"negative binomial" = , 
				        "normal" = dnorm, 
				        #"Poisson" = ,
				        #"t" = ,
				        "weibull" = dweibull # ,
				        #"power" = , ##############
				        #"pareto" =  ############## DO THESE
			   )

  fitModel <- function(modelName, x){
    if ( modelName=="power" | modelName=="pareto" ){
	    fit <- fitdistr(x, PDFs$modelName) ### If this doesn't work, use PDFs[modelName]
	  }
	  else {
      fit <- (fitdistr(x, modelName) )
	  }
	  return (fit)
  }
  
  fitList <- lapply(modelNames, fitModel, x=obs)
  names(fitList) <- modelNames
  
  xmin <- min(obs)
  xmax <- max(obs)
  xseq <- seq(xmin, xmax, (xmax-xmin)/2000)
  plot(density(obs), log="y", main = paste(label, "PDF fits"), xlab=label, ylab=paste("Probability density of",label))
  
  for (model in modelNames) {
    lines (xseq, do.call(PDFs[[model]], c(list("x"=xseq), as.list(fitList[[model]]$estimate)) ), type="l" )
  #  curve( call("PDFs[[model]]", as.list(c(x=x, fitList[[model]]$estimate)) ), add=TRUE ) #### Need to do different colors!
  } ##################### WHAT IS A BETTER WAY TO DO THIS?
  
  AIClist <- list()
  for (model in modelNames){
    AIClist[model] <- AIC( logLik(fitList[[model]]), k=length(fitList[[model]]$estimate) )
  }
  
  ########## LRT test?
  
  return ( list( "fitList"=fitList, "AIClist"=AIClist ) )
}

hazardAnalysis <- function(obs, label){
  info <- fitModels(obs, c("exponential", "weibull"), label)
  # Do likelihood ratio test for exp is null and return p-value
  return (info)
}

powerLawAnalysis <- function(obs, label){
  # Do a shapiro-wilk test for normality and log-normality
  # Do a K-S thingy
  fitModels(obs, c("lognormal", "power"), label)
  # Do I have to think about having a min cut-off?
}


###### OLD COMMENTS HERE
# file created 9-19-12 by Brendon Fuhs
# last updated 10-13-12
# 
### Methods to use:
# 
# ModelTest: Basic likelihood-based model testing
# printStats: Descriptive statistics
# empPlot: Visualization of empirical distributions
# 
### non-default packages used:
#
# bbmle, moments
# 



########### Continuous distributions and corresponding LogL functions

# PDF: dexp
# CDF: pexp
LogLExp <- function(a, obs){
  n<-length(obs)
  LogL=n*log(a) - a*sum(obs)
  if (is.na(LogL)) 
    LogL <- -1e+100
  if (LogL <= -1e+100)
    LogL <- -1e+100
  return (-LogL)
}

# PDF: dweibull
# CDF: pweibull
LogLWeib <- function(a,b,obs){
  n <- length(obs)
  LogL <- n*log(a/(b^a)) + (a-1)*sum(log(obs)) - sum(obs^a)/(b^a)
  if (is.na(LogL)) 
    LogL <- -1e+100
  if (LogL <= -1e+100) 
    LogL <- -1e+100
  return (-LogL)
}

# PDF:
paretoPDF <- function(x,a,b){
  Prob = (b-1) * a^(b-1) * x^(-b)
  return (Prob*(Prob >= 0))
}

LogLPower <- function(a,b,obs){ 
  n<-length(obs)
  LogL <- n*log((b-1)*a^(b-1)) - b*sum(log(obs))
  
  if (is.na(LogL)) 
    LogL <- -1e+100
  if (LogL <= -1e+100) 
    LogL <- -1e+100
  return (-LogL)
}


########### Discrete distributions and corresponding LogL functions

# PDF: dgeom
# CDF: pgeom
LogLGeom <- function(a, obs){ # a is a probability
  n = length(obs)
  # Lik = (1-a)^sum(obs-1) * a^n
  LogL = sum(obs-1)*log(1-a) + n*log(a)
  if (is.na(LogL)) 
    LogL <- -1e+100
  if (LogL <= -1e+100) 
    LogL <- -1e+100
  return (-LogL)
}


# books.google.com/books?isbn=185233939X page 17
# changed parameterization a little
discWeibPMF <- function(x,a,b){
  Prob = (1-a)^(x^b) - (1-a)^((x+1)^b)
  return (Prob*(Prob >= 0))
}

LogLDiscWeib<- function(a,b,obs){
  # LogL = sum(log((1-a)^(obs^b) - (1-a)^((obs+1)^b)))
  # Not sure how to simplify this
  # Try:
  LogL = log(1-a) * sum(obs^b) + sum(log( 1 - (1-a)^((obs+1)^b - obs^b)))
  if (is.na(LogL)) 
    LogL <- -1e+100
  if (LogL <= -1e+100) 
    LogL <- -1e+100
  return (-LogL)
}

############ Putting things together as lists for use in Model Test

modelNames = c("exp",
               "weib",
               "geom",
               "discWeib",
               "power")

LogLs = c(LogLExp,
          LogLWeib,
          LogLGeom,
          LogLDiscWeib,
          LogLPower)

PDFs = c( dexp,
          dweibull,
          dgeom,
          discWeibPMF,
          paretoPDF)

names(PDFs)<-names(LogLs)<-modelNames

####### MODEL TESTING
# This takes an array or datatable or matrix, and a vector of model names
# first column of array is the dependent variable
#
ModelTest <- function(dataArray, modelChoices){
  # use tryCatch or withCallingHandlers for exception handling
  
  # Clean up the NA's and the zeros maybe and return info
  
  ############ CHANGE THESE TO LISTS
  # Check dimensionality of dataArray and proceed accordingly
  ## This will determine whether mle2 takes a "fixed=" entry
  if ( class(dataArray)=="data.frame" || class(dataArray)=="matrix" ){
    depVar = rep(list(dataArray[,1]), length(modelChoices))
    indVars = rep(list(dataArray[,-1]), length(modelChoices))
  }
  else{
    depVar = rep(list(dataArray), length(modelChoices))
    indVars = rep(list(NULL), length(modelChoices))
  }
  
  ############# I need to find a way to write these as functions 
  ############# and then apply them only to used models
  initialValues = list( 6 ) #Put initial value vectors in here.
  LBounds = list(4) # put lower bounds vectors here
  UBounds = list(5) # put upper bounds vectors here
  
  names(depVar)<-names(indVars)<-modelChoices
  names(initialValues)<-names(LBounds)<-names(UBounds)<-modelChoices
  
  # Some of this should be redundant, but that's okay.
  argsFrame <- data.frame(list( minuslog1 = LogLs[modelChoices],
                                start = initVals[modelChoices], ###Each a list
                                method = 7,###### vector of "L-BFGS-B"
                                optimizer = 8, ###### vector of "optim"
                                fixed = indVars[modelChoices],
                                data = depVar[modelChoices], 
                                lower = 11, ####
                                upper = 12 )) #######
                                
  fitsList <- mlply(argsFrame,mle2)

  return (fitsList) ##(for now)
}

Test1 <- function(diffs){
  meanDiff <- mean(diffs)
  
  expFit <- mle2(LogLExp, start=list(a=1/(meanDiff+1) ),
                 method="L-BFGS-B", data=list(obs=diffs),
                 optimizer="optim", lower=1e-6)
  
  weibFit <- mle2(LogLWeib, start=list(a=1, b=meanDiff),
                  method="L-BFGS-B",data=list(obs=diffs),
                  optimizer="optim", lower=c(1e-6,1e-6))
  
  print (summary(expFit))
  print (summary(weibFit))
  #print(profile(expFit))
  #print(confint(expFit))
  
  print(pValFromLRT(logLik(expFit)[1], logLik(weibFit)[1], 1))
  
  # xx = c(1:145)
  # plot(xx, ecdf(diffs)(xx))
  # plot(xx, empPDF(diffs)(xx))
  
  hist(diffs, freq=FALSE, # breaks = seq(0,max(diffs),5),
       xlab="Days between declarations", ylab="Probability Density",
       main="Distribution of number of days between OFDA declarations",
       sub="blue=Exponential, green=Weibull, red is smoothed non-parametric")
  lines(density(diffs), col = "red")
  # plot(density(diffs), col = "red", log="y") Can omit top two and use this if log plot desired
  curve(dexp(x,coef(expFit)), col="blue", add=TRUE)
  curve(dweibull(x,coef(weibFit)[1],coef(weibFit)[2]), col="green", add=TRUE)
}

Test1Discrete <- function(diffs){
  meanDiff <- mean(diffs)
  
  geoFit <- mle2(LogLGeom, start=list(a=1/(meanDiff+3)),
                 method="L-BFGS-B", data=list(obs=diffs),
                 optimizer="optim", lower=1e-6, upper=1-(1e-6))
  
  # This isn'converging properly
  discWeibFit <- mle2(LogLDiscWeib, start=list(a=1/(meanDiff+3), b=1.4),
                      method="L-BFGS-B",data=list(obs=diffs),
                      optimizer="optim", lower=c(1e-6,1e-6), upper=c(1-(1e-6), 1e10))
  
  print (summary(geoFit))
  print (summary(discWeibFit))
  
  print(pValFromLRT(logLik(geoFit)[1], logLik(discWeibFit)[1], 1))
  
  # Apparently NOT dpois
  #dgeom
  
  hist(diffs, freq=FALSE, breaks = seq(0,max(diffs),1),
       xlab="Days between declarations", ylab="Probability Density",
       main="Discrete distribution of number of days between OFDA declarations",
       sub="blue=Geometric, green=discrete analog of Weibull (probably broken)")
  #lines(density(diffs), col = "red")
  # plot(density(diffs), col = "red", log="y") Can omit top two and use this if log plot desired
  lines(dgeom(1:max(diffs),coef(geoFit)), col="blue")
  lines(discWeibPMF(1:max(diffs),coef(discWeibFit)[1],coef(discWeibFit)[2]), col="green")
}

# Divide cost by a power of 10000 so it doesn't overwhelm things
Test2 <- function(obs){
  
  # First, OLS on logs
  obs=obs[obs!=0 & !is.na(obs)]
  

  
  
  # Now should do something better.
  
  powerFit <- mle2(LogLPower, start=list(a=min(obs)/2, b = 2),
                   method="L-BFGS-B", data=list(obs=obs),             #####????
                   optimizer="optim", lower=c(1e-6 ,1+(1e-6)), upper=c(min(obs)-1e-10, 1e20)) 
  
  print(summary(powerFit))
  
  #### This is a stupid bandwidth trick that probably doesn't make sense.
  plot(density(obs, bw=exp(4.2*x)), col = "red", log="xy", ylim = c(1e-8,1),
       xlab="Cost/10000", ylab="Probability Density",
       main="Distribution of cost per OFDA country-response, log-log",
       sub="blue = power law, red = non-parametric")
  xx = seq(10,max(obs)+10,max(obs)/1000)
  lines(xx,paretoPDF(xx,coef(powerFit)[1],coef(powerFit)[2]), col="blue")
  
  
  #####OLS POWER LAW CRAP
  empCDF = ecdf(obs)
  obs1 = obs[obs!=max(obs)] # Get rid of max to keep logs from breaking
  Xlogs = log(obs1)
  Ylogs = log(1 - empCDF(obs1))
  
  loggyFit <- lm(Ylogs ~ Xlogs)
  print(summary(loggyFit))
  
  plot(Xlogs,Ylogs, xlab="log(cost/10000)", ylab="log(1 - empiricalCDF)",
       main="OLS applied to logs: cost of each country-response",
       sub="blue=OLS fit to logs")
  abline(loggyFit, col="purple")
  
}

pValFromLRT <- function(LogL0, LogL1, paramNumDiff){
  Dstat <- -2*LogL0 +2*LogL1
  
  return (1 - pchisq(Dstat, paramNumDiff))
}


# empCDF<-ecdf(obs)

# Shapiro-Wilk test for normality and log-normality
Test0 <- function(diffs){
  diffs=diffs[diffs!=0 & !is.na(diffs)]
  
  print("Normal: ")
  print(shapiro.test(diffs))
  print("Log-normal: (Okay to test this way?) ")
  print(shapiro.test(log(diffs))) ## Is this how to do the log-normality?
  
}


plotEmpHaz <- function(somedata){
  somedata = somedata[somedata!=0 & !is.na(somedata)] # Do I need this?
  
  empCDF = ecdf(somedata)
  empPDF = density(somedata)
  
  plot(empPDF$x,empPDF$y/(1-empCDF(empPDF$x)), type="l")
  
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


