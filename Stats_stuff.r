##### Stats_stuff.r
#
# Brendon Fuhs
# updated 11-19-12
#
# getStats(data)                      ### Descriptive Statistics
# BROKEN epochComparison(epochal dates, differences, breaks) ### Stationarity Testing
# ^^^ make call Multi_sets.r
# plotEmpirical(data, data label)     ### Plot nonparametric estimate of distribution
# fitModels(data, vector of distribution names, data label) ### Test models
# hazardAnalysis(data, data label)    ### Weibull Test
# powerLawAnalysis(data, data label)  ### Power Law Analysis
# analyzeDurations(data) ### Analyze durations how???
# 
###################### Another way of doing power law?, LRT????
######
# Note that these currently are only working for univariate data
# default libraries used include stats
# non-default libraries used include...
# packages that need to be downloaded include moments

# library(stringr)
library(moments)
# library(bbmle)
# library(plyr)
library(MASS)
library(ggplot2) ### Not used yet

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
  if (length(x)<2){
    return (NULL)
  }
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
  
  paretoPDF <- function(x,a,b){
    Prob = (b-1) * a^(b-1) * x^(-b)
    return (Prob*(Prob >= 0)*(b >= 1))
  }
  
  PDFs <- list( "beta" = dbeta,
                "binomial" = dbinom,
                "cauchy" = dcauchy,
				        "chi-squared" = dchisq,
				        "exponential" = dexp, 
				        "f" = df, 
				        "gamma" = dgamma,
				        "geometric" = dgeom, 
				        "log-normal" = dlnorm, 
				        "lognormal" = dlnorm, 
				        "logistic" = dlogis, 
				        "negative binomial" = dnbinom, 
				        "normal" = dnorm, 
				        "Poisson" = dpois,
				        "t" = dt,
				        "weibull" = dweibull,
				        "power" = paretoPDF,
				        "pareto" =  paretoPDF
			   )

  fitModel <- function(modelName, x){
    if ( modelName=="power" | modelName=="pareto" | modelName=="binomial" ){ ## won't work for binomial
	    fit <- fitdistr(x, PDFs[[modelName]], start = list(a=0.1,b=1.2)) ### If this doesn't work, use PDFs[modelName] or PDFs$modelName
	  } 
	  else {
      fit <- (fitdistr(x, modelName) )
	  }
	  return (fit)
  }
  
  fitList <- lapply(modelNames, fitModel, x=obs)
  names(fitList) <- modelNames
  
  
  ###### REWRITE THIS PLOTTING STUFF SO IT'S GGPLOT2
  xmin <- min(obs)
  xmax <- max(obs)
  xseq <- seq(xmin, xmax, (xmax-xmin)/2000)
  plot(density(obs), log="y", main = paste(label, "PDF fits"), xlab=label, ylab=paste("Probability density of",label))
  
  for (model in modelNames) {
    lines (xseq, do.call(PDFs[[model]], c(list("x"=xseq), as.list(fitList[[model]]$estimate)) ), type="l" )
  #  curve( call("PDFs[[model]]", as.list(c(x=x, fitList[[model]]$estimate)) ), add=TRUE ) #### Need to do different colors!
  } ##################### WHAT IS A BETTER WAY TO DO THIS?
  
  AIClist <- list()
  chiSquareList <- list()
  x <- x[!is.na(x)]
  for (model in modelNames){
    chiSquareList[model] <- chisq.test(x, p=do.call(PDFs[[model]], c(list("x"=x), as.list(fitList[[model]]$estimate)) ), rescale.p=TRUE)
    AIClist[model] <- AIC( logLik(fitList[[model]]), k=length(fitList[[model]]$estimate) )
  }
  
  ########## LRT test?
  
  
  return ( list( "fitList"=fitList, "AIClist"=AIClist, "chiSquareList"=chiSquareList) )
}

hazardAnalysis <- function(obs, label){
  obs <- obs[obs>0]
  info <- fitModels(obs, c("exponential", "weibull"), label)
  ### Do likelihood ratio test for exp is null and return p-value
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
  
  # Use  diffTimes and breaks to create a factor vector thingy
  
  # Call makeTable
  
  # Do comparisons based on that.
  
  # compare the stats for each chunk
  
  # compare fits/tests for each chunk
  
  # Is there a way to do this all statisticsy and stuff?
  
}

analyzeDurations <- function(obs, label){
  obs <- obs[obs>=0]
  ########## How to test for clustering, bimodality?
  ## Maybe I should just plot the distributions for now and look at them
  info <- fitModels(obs, c("exponential", "weibull", "lognormal"), label) #, "power"
  # Do likelihood ratio test for exp is null and return p-value
  return (info)
}

fitPower <- function(obs,label){
  obs <- as.numeric(obs)
  obs <- obs[!is.na(obs)]
  obs <- obs[obs>0]
  if (length(obs)<3){
    return (NULL)
  }
  empCDF = ecdf(obs)
  obs1 = obs[obs!=max(obs)] # Get rid of max to keep logs from breaking
  Xlogs = log(obs1)
  Ylogs = log(1 - empCDF(obs1))
  
  loggyFit <- lm(Ylogs ~ Xlogs)
  print(summary(loggyFit))
  
  plot(Xlogs,Ylogs, xlab= paste("log ", label),
       ylab="log(1 - empiricalCDF)",
       main= paste("OLS applied to logs: ", label),
       sub="blue=OLS fit to logs")
  abline(loggyFit, col="blue")
  
  print(c(label, " has median..."))
  print(median(obs))
  print(" ")
  expon <- 0
  #print(c(label, " has exponent..."))
  #print(expon)
  #print(" ")
  # Will need to test how good the fit is.
}

