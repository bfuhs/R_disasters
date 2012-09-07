#
# disasterTest2.r
# created 8-4-12, yet another attempt at this
# 
# This time:
# think about likelihood regression 
# use Surv
# avoid named objects?

## List pdfs (or maybe prompt for their input?)
# For sure:
# exponential
# Weibull
# pareto
# That 3-parameter one in the book (will need to define)
# Other ones in the book (if there are any others testable)
#
# Maybe:
# Floating or piecewise power law
# lognormal
# gamma
# stretched exponential

getData<-function(){  # This depends on what I have and what I want to measure
  # input raw data
    # consider having options to read from file, clipboard, etc

  # Have user indicate which columns are which?
  
  # prepare and clean data
    # Get data into the correct Type
    # Remove null values when/if applicable
    # Maybe remove zeros when/if applicable
  
  # output names lists and maybe datatables of stuff to be processed
}

findFits<-function(obs){#,modelNums){  # Change default modelnums if more models

  parGuesses<-list(
      mean(obs),
      c(mean(obs),1))
  
  LBounds<-list(
      1e-5,
      c(1e-5,1e-5))
                       
  LogList<-list(function(theta, obs){
                  n<-length(obs)
                  LogL=n*log(theta) - theta*sum(obs)
                  return (-LogL)
                },
                function(theta, obs){
                  a=theta[1]
                  b=theta[2]
                  n<-length(obs)
                  LogL <- n*log(a*(b^-a)) + (a-1)*sum(log(obs)) - sum(obs^a)/(b^a)
                  return (-LogL)
                })
  
  MLEfunction <- function(parGuess, LBound, LogL){
    return(optim(parGuess,
                 LogL,
                 obs=obs, 
                 method="L-BFGS-B",
                 lower=LBound))
    }

  MLEinfo<-as.list(mapply(MLEfunction,
                  parGuesses, LBounds, LogList))
  
  # This is kicking out a weird format even though I attempted to listify it
  
  AICfunction<-function(MLEinfo){
    n=length(obs)
    k=length(MLEinfo$par)
    AIC=2*k-2*MLEinfo$value
    AICc=AIC+2*k*(k+1)/(n-k-1)
    return(c(AIC,AICc))
  }
  #AICinfo<-lapply(MLEinfo,AICfunction)
  AICinfo<-apply(MLEinfo,2,AICfunction)   # The "2" means to apply to columns
  # but using apply I can apply only to some margins here I guess?
  
  # p. 553
  # -2*log((likelihood ratio is lambda)) follows chi square with diff in params
  # but that requires a null hypothesis, right? (Do I have to use Poisson? Can I have it be any of my models?)
  # Maybe this is *like* a goodness of fit test
  # Don't implement this yet I don't think.
                       
  return(list(MLEs=MLEinfo,AICs=AICinfo))
}

plotFits<-function(obs,fitInfo){
  # Need to extract stuff from inputs
  
  # Need to figure out what to plot for both x AND y sets,
  # and also if multiple x value sets!
    # Maybe start with just one set and think about how to add later
  
  
  pdfs<-list(dexp,
             dweibull)
  cdfs<-list(pexp,
             pweibull)
  hazManual<-list(
              )
  hazAuto<-function(pdf,cdf,y){
    return(pdf(y)/(1-cdf(y)))
  }
  
  empCDF<-ecdf(obs)
  #can use plot.ecdf directly on this
  
  empPDF<-function(obs){ # use Kaplan-Meier
    # slope of CDF evaluated at midpoints
    obsvals=unique(obs)
    # xpts: take unique(x) and halve them
    xpts=rowMeans(cbind(obsvals[-1],obsvals[-length(obsvals)]))
    
    # ypts: slope evaluated at average of CDF(unique(x))
    # rise over run
    rise=empCDF(obsvals)[-1] - empCDF(obsvals)[-length(obsvals)]  #should I put brackets inside?
    run=obsvals[-1]-obsvals[-length(obsvals)]
    ypts=rise/run
    
    ### use approxfun (or stepfun??) or whatever to make a function
    # see previous stuffs
    return (approxfun(xpts, y=ypts, yleft=0, yright=0))
    
    # But might I just want the points?
    # or make some kind of step function
    # or use Surv
    # I'll definitely want to use Surv eventually I think??
  }
  empHaz<-function(obs){
    return (empPDF(obs)/(1-empCDF(obs)))
  }
  
  # plot stuff
}

# Suppose obs has Y, X1, X2, ...
# How do I do that with MLE?
# X1,X2 are ind vars - I don't care about their distribution
# Does that mean I treat them like known/fixed parameters?
# Then just write likelihood function to strip X1,X2... vectors from obs
# REwrite already-existing likelihood functions to ignore extra columns(/rows?) of obs
# 

# Do empirical stuffs apply or have plots?
# 
