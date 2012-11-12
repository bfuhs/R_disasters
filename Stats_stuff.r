##### Stats_stuff.r
# Brendon Fuhs
# updated 11-5-12
#
### Descriptive Statistics
# getStats(data)
#
### Stationarity Testing
# epochComparison(epochal dates, differences, breaks) ## NOT SURE HOW TO HANDLE THIS
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
# library(plyr)
library(MASS)
library(ggplot2) ###

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
  obs=obs[obs!=0 & !is.na(obs)]
  
  ### Should I do Anderson-Darling or other tests?
  ### NEED POWER LAW CDF powerCDF()
  # Do I have to think about having a min cut-off?
  
  print("Shapiro-Wilk Test (low p-value means not log-normal)")
  print("Normal: ")
  print(shapiro.test(obs))
  print("Log-normal: ")
  print(shapiro.test(log(obs))) ## Is this how to do the log-normality?
  print(" ")
  
  something <- fitModels(obs, c("lognormal", "power"), label)
  print("Kolmogorov-Smirnov Test")
  print(ks.test(obs, "powerCDF", something$something$estimate))
}

### Stationarity Testing
epochComparison <- function(longTimes, diffTimes, breaks){
  
  # Create chunks
  
  # compare the stats for each chunk
  
  # compare fits/tests for each chunk
  
  # Is there a way to do this all statisticsy and stuff?
  
}


