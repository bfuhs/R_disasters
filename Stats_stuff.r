##### Stats_stuff.r
#
# Brendon Fuhs
# updated 2-11-13
# 
# This module contains functions to conduct statistical analyses of univariate data
#
# NON-DEFAULT PACKAGES REQUIRED
# moments, ggplot2
#
# FUNCTIONS
# getStats(data)                      ### Table of descriptive Statistics
# plotEmpirical(data, data label)     ### Plot nonparametric estimate of distribution
# fitModels(data, vector of distribution names, data label) ### Test models
# hazardAnalysis(data, data label)    ### Weibull Test
# powerLawAnalysis(data, data label)  ### Power Law Analysis
#  PLACEHOLDERS FOR NOW...
# epochComparison(epochal dates, differences, breaks) ### Stationarity Testing
# analyzeDurations(data)              ### Analyze durations how???
# 
# USAGE
# 


library(moments)
library(MASS)
library(ggplot2)


# descriptive stats of vector x
# (requires moments package)
getStats <- function(x){
  x<-as.numeric(x)
  xStats <- c( mean(x, na.rm=TRUE), #, took out na.rm=TRUE - didn't make a difference
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
## Apparently "label" can't have any spaces or it breaks!
plotEmpirical <- function(x, varName){ # WHYYY is the variable name
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if (length(x)<2){
    return (NULL)
  }
  x <- data.frame(x)
  names(x) <- varName
  assign("xGLOBAL", x, envir = .GlobalEnv)
  assign("varNameGLOBAL", varName, envir = .GlobalEnv)

  CDFplot <- ggplot(xGLOBAL, aes_string(x=varNameGLOBAL)) + stat_ecdf()
  CDFplot <- CDFplot + ggtitle(paste("Est. cumulative probability of ", varNameGLOBAL)) + ylab("cumulative probability")
  print(CDFplot)
  # Need to label y

  PDFplot <- ggplot(xGLOBAL, aes_string(x=varNameGLOBAL)) + geom_density() # + scale_x_log10()
  PDFplot <- PDFplot + ggtitle(paste("Est. probably density of ", varNameGLOBAL))
  print(PDFplot)
  
  # hazard function plot
  empPDF <- density(x[,1])
  hazData <- data.frame("x"= empPDF$x,"y"= empPDF$y / (1 - ecdf(x[,1])(empPDF$x)) )
  HAZplot <- ggplot(hazData, aes(x=x,y=y)) + geom_line()
  ### AES ENV!!! (Why is it apparently okay now??)
  HAZplot <- HAZplot + ggtitle(paste("Est. hazard function of ", varNameGLOBAL)) + ylab("hazard") + xlab(varNameGLOBAL)
  print(HAZplot)
  # Could I have used geom_smooth with this instead?
}

## Fits to distributions. Not entirely working
fitModels <- function(obs, modelNames, label){
  obs <- as.numeric(obs)
  obs <- obs[!is.na(obs)]
  obs <- obs[obs!=0] #### maybe not
  print(length(obs))#############################
  
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
    print(length(x))##############
    print(modelName)########
    print("Got here yay")#########
    #if (length(x) < 5){
    #  return (NULL)
    #}
    if ( modelName=="power" | modelName=="pareto" | modelName=="binomial" ){ ## won't work for binomial
	    fit <- fitdistr(x, PDFs[[modelName]], start = list(a=0.1,b=1.2)) ### I GUESS THIOS WORKED???!?!?! (for hazard analysis)
	  } 
	  else {
      fit <- (fitdistr(x, modelName, lower=c(0.01, 0.01)) ) ######## Do I need start values here?
      print("Got here too yay")#########
	  }
	  return (fit)
  }
  
  fitList <- lapply(modelNames, fitModel, x=obs) # sapply breaks this later?
  names(fitList) <- modelNames
  print("Did we get here?")#####################3
  
  xmin <- min(obs)
  xmax <- max(obs)
  obsData <- data.frame(obs)
  names(obsData) <- "x"
  assign("obsGLOBAL", obsData, envir = .GlobalEnv)
  assign("obsNameGLOBAL", label, envir = .GlobalEnv)
  print("What about here?")##########################
  p <- ggplot(NULL, aes(x=x, color=legend)) # FIX THIS?
  p <- p + geom_density(data = data.frame(x=obsGLOBAL[,1], legend=factor(1))) + xlab(obsNameGLOBAL)
  colors <- 1
  print("And now to add spiffy lines...")####################33
  for (model in modelNames) {
    colors <- c(colors, tail(colors,1)+1)
    p <- p + stat_function(data = data.frame(x=xmin:xmax, legend=factor(tail(colors,1))), fun = PDFs[[model]], args=fitList[[model]]$estimate)
  }
  print("spiffy up some stuff...")###########################
  p <- p + ggtitle(paste("Probability density fits of ", obsNameGLOBAL))
  p <- p + scale_colour_manual(values = colors, labels = c("empirical PDF", modelNames)) ### DOES NOTHING
  print(p)
  ### And what about confidence intervals?
    ## I do have standard error (se). Would that be appropriate to use?
      # But then I have sd for multiple parameters
        # Put this on back-burner for now.

  # DO AIC STUFF HERE
  ## Changed these from "<- list()"
  #AIClist <- c()
  #chiSquareList <- c()
  #print(length(obs))################
  #obs <- obs[!is.na(obs)]
  #print(length(obs))################
  #for (model in modelNames){
  #  chiSquareList[model] <- chisq.test(obs, p=do.call(PDFs[[model]], c(list("x"=obs), as.list(fitList[[model]]$estimate)) ), rescale.p=TRUE)
  #  AIClist[model] <- AIC( fitList[[model]] ) ## Took out logLik(###) , k=length(fitList[[model]]$estimate)
  #}  ## Used to be return(list(####))
  return ( c( "fitList"=fitList, "AIClist"=NULL) )
} #### NEED TO RETURN STUFF IN NAMES VECTOR OR ARRAY

hazardAnalysis <- function(obs, label){
  obs <- obs[obs>0]
  print (length(obs))#################
  print (label)#######################
  info <- fitModels(obs, c("exponential", "weibull"), label)
  
  # Likelihood Ratio Test, exp is null hyp, weib is alt hyp, 1 df
  logL_0 <- info$fitList.exponential$loglik
  logL_1 <- info$fitList.weibull$loglik
  
  LRTstat <- -2*(logL_0 - logL_1)
  CHISQval <- function(alpha){
    return(qchisq(1-alpha, 1)) ## ???
  }
  LRT <- function(alpha){
    if (LRTstat > CHISQval(alpha)){
      return ("reject exp")
    }
    return ("don't reject exp")
  }
  output <- c(info$fitList.exponential$estimate, info$fitList.weibull$estimate, LRTstat, LRT(.05), LRT(.01), LRT(.001))
  names(output) <- c("exp: rate", "weib: shape", "weib: scale", "LRT stat", "alpha .05", "alpha .01", "alpha .001")
  return (output)         
}

# Check the results of this against plfit.r and plpva.r,
#  which may use some different methodologies
# Do I have to think about having a min cut-off?
powerLawAnalysis <- function(obs, label){
  obs = obs[obs>0 & !is.na(obs)]
  CDFfn = ecdf(obs)
  obs <- obs[obs != max(obs)] # Get rid of the max value?
        # Do I really need to eliminate zeros?
        # Do I need unique values?
        # Do I need to make sure there are 3 or more data points?
  xlogs = log(obs)
  ylogs = log(1 - CDFfn(obs))
  # x axis is log(obs)
  # y axis is log(1-CDF(obs))
  loggyFit <- lm(ylogs ~ xlogs)
  a = loggyFit$coefficients[[1]]
  # intercept will be a prime (log(a) I think)
  a = exp(a) #(intercept)# ?????
  # coefficient will be -(b-1) or 1-b I think
  b = loggyFit$coefficients[[2]]
  b = 1-b 
  Rsquared <- summary(loggyFit)$r.squared
  
  ### LABELS NOT WORKING
  xlabel <- paste("Log of ", label)
  ylable <- paste("Log of Complementary Cumulative Probability of ", label)
  title <- paste("Power Law fit of ", label)
  p <- ggplot(data.frame(xlabel=xlogs, ylabel=ylogs), aes(xlabel,ylabel))
  p <- p + geom_point() + geom_smooth(method=lm) # Should be same as fit
  p <- p + ggtitle(title) ### THIS MIGHT BREAK FOR STUPID REASONS
  print(p)
  
  # Tests to include: 
  
  # R^2 (bad, common)
  # Hill estimator (unstable for small sample)  # NEED THIS
  # Anderson-Darling (Type I error risk)        # NEED THIS
  # Kolmogorov-Smirnov (Insensitive to upper tail values, Type II error risk)
  # Shapiro-Wilk Test
  
  # Shapiro-Wilk Test (low p-value means not log-normal)
  shapWilkPval <- shapiro.test(log(obs))$p.val

  #print(ks.test(obs, "powerCDF", something$something$estimate))
  
  return (c("a"=a, "b"=b, "Shapiro-Wilk p-value"=shapWilkPval, "R squared"=Rsquared))
  
  # Not implementing this..
  # something <- fitModels(obs, c("lognormal", "power"), label)
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


