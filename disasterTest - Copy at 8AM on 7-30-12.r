# 
# disasterTest.r
# 7/26/12
# 
# functions: dataPrep, timeSeries, 
#    rawStats, fitInfo, empFunctions, plotStuff
# 
# fitinfo(x)
# takes data, returns list of mle2 fit objects
#

library(bbmle)
library(VGAM)

fitInfo <- function(x){
  # input data (doesn't have to be sorted)
  # outputs data frame of maxed likelihood and parameters for all models
  # format: model# is row, pars vector, maxed lik, convergence?
  
  ## a bunch of loglikelihood functions
  # Let's see if there'a any of these hat can be simplified.
  # Or looked up and copied from the Internet
  
  # Exponential
  # H(x) = k : k>0
  # p(x) = k*exp(-k*x) : k>0
  # 1 parameter
  LogLExp <- function(theta, x){
    k <- theta[1]
    n<-length(x)
    LogL <- n*log(k) - k*sum(x)
    return (-LogL)
  }
  
  ## Not Log-linear? Linear Failure Rate Distribution: OMIT; Only include if Weibull signals right
  ## H(x) = a + b*x : a,b>0
  ## P(x) = (a + b*x)*exp(-x*(a + (b/2)*x)) : a,b>0
  ## 2 parameters
  #LogLNotLogLin <- function(theta, x){
  #  a <- theta[1]
  #  b <- theta[2]
  #  LogL <- sum(log(a+b*x))-sum(x*(a+b*x/2))
  #  return (-LogL)
  #} # Do I need real log-linear or not?
  
  # Weibull
  # H(x) = a*(b^-a)*(x^(a-1)) : a>0, b>0
  # p(x) = a*(b^-a)*(x^(a-1))*exp(-(x/b)^a) Book notates differently
  # 2 parameters
  LogLWeib <- function(theta, x){
    a <- theta[1]
    b <- theta[2]
    n<-length(x)
    LogL <- n*log(a*(b^-a)) + (a-1)*sum(log(x)) - sum(x^a)/(b^a)
    return (-LogL)
  } 
  
  # Log-normal
  # H(x) = NOT normal(mu, sigma2). Something crazier/weirder
  # P(x) = something over x *exp(normal)
  # 2 parameters
  LogLLogNorm <- function(theta, x){
    mu <- theta[1]
    sigma2 <- theta[2]
    n=length(y)
    LogL <- n*log((2*pi*sigma2)^(-.5))-sum(log(x))-(1/(2*sigma2))*sum((log(x)-mu)^2)
    return(-LogL)
  }
  # What should I use for normal Hazard? Log-f(erf)?
  
  # Power Law
  # H(x) = b/x : # b>0, b<2??, x>=xmin
  # p(x) = b*(xmin^b)/(x^(b+1))  # b>0, b<2??, x>=xmin
  # One or two parameters
  LogLPower <- function(theta, x, xmin){
    b <- theta[1]
    n<-length(x)
    LogL <- n*log(b*(xmin^b)) - (b+1)*sum(log(x))
    return (-LogL)
  }# or should  do/say pareto?

  # Gamma, I think same as power law with exponential cut-off. MLE might be bimodal
  # H(x) = Look it up. It's crazy/weird.
  # P(x) = 1/(gamma(a)*b^a) * x^(a-1) * exp(-x/b) : b>0
  # 2 parameters
  LogLGamma<- function(theta, x){
    a <- theta[1]
    b <- theta[2]
    n<-length(x)
    LogL <- n*log(1/(gamma(a)*b^a) ) + (a-1)*sum(log(x)) - sum(x)/b
    return (-LogL)
  } 

  # Should maybe write exp cut-off separately? With xmin instead of zero lower bound?

  ## Stretched exponential
  ## H(x) = 
  ## P(x) = 
  #LogLStretchedExp<- function(theta, x){
  #
  #}

  # Part exponential, part power law
  # H(x) = 
  # P(x) = 
  # 2 parameters
  LogLPartPower <- function (theta,x){
    xmid <- theta[1]
    b <- theta[2]
    xexp <- x[x<xmid]
    xpower <- x[x>=xmid]
    return ( LogLExp( (b+1)/xmid ,xexp) + LogLPower(theta ,xpower, xmid) )
  } # Should I combine with Weib too?

  xbar=mean(x)
  xmed=median(x)
  xsd=sd(x)
  n-length(x)
  xmin=min(x)
  MLEguesses<- list( 	# These are known ML estimators and guesses for initial values
    xbar,							# MLE
    c(1,xbar),   						# using above; ORDER MAY CHANGE
    c( sum(log(x))/n, sum((log(x) - sum(log(x))/n)^2)/n), 	# MLE; muhat, sigma2hat
    1+n/sum(log(x/xmin)),		# xmin			# MLE	
    c( (xbar/xsd)^2, xsd^2/xbar),				# moments
    # ???							# moments??
    c(xmed, 1+n/sum(log(x[x>=xmed]/xmed))/2)		# guess
  )
    # WHAT??
  LBounds <- list(
    1e-4,
    c(1e-4,1e-4),
    c(-Inf,1e-4),
    1e-5, # xmin specified separately, but should be greater than 0
    c(1e-4,1e-4),
    # ???,
    c(1e-4, 10e-4)
  )
  
  names(LBounds)<-names(MLEguesses) <- list( 
    "Exp",
    "Weib",
    "LogNorm",
    "Pareto", # or maybe power law?
    "Gamma", # Power law with exponential cut-off
    # "StretchExp",
    "PartPower"
  )
  
  ## a bunch of calls to the optimizer or bbmle
  
  fits <- list(	# Might need some of this stuff to be lists
    mle2(LogLExp, start=list(theta=MLEguesses$Exp), method="L-BFGS-B", lower=LBounds$Exp, data=list(x=x)),
    mle2(LogLWeib, start=list(theta=MLEguesses$Weib) , method="L-BFGS-B", lower=LBounds$Weib, data=list(x=x) ),
    mle2(LogLLogNorm, start=list(theta=MLEguesses$LogNorm) , method="L-BFGS-B", lower=LBounds$Lognorm, data=list(x=x) ),
    mle2(LogLPower, start=list(theta=MLEguesses$Pareto) , method="L-BFGS-B", lower=LBounds$Pareto, data=list(x=x, xmin = min(x))),
    mle2(LogLGamma, start=list(theta=MLEguesses$Gamma) , method="L-BFGS-B", lower=LBounds$Gamma, data=list(x=x) ),
    #	mle2(LogLStretchedExp, MLEguesses$StretchExp , method="L-BFGS-B", data=x ),
    mle2(LogLPartPower, start=list(MLEguesses$PartPower) , method="L-BFGS-B", lower=LBounds$PartPower, data=list(x=x) )
  )
  
  names(fits)<-names(MLEguesses)
  return (fits)
}


# Can I suppress some fits prior to input or will it screw up the names?
plotStuff <- function(empFns,fits,xx,logstring=NULL){ #xx= MY DOMAIN!!!!!!
  # This function takes a list of empirical functions,
  # a list of fit objects, and plots one on top of the other
  # with logs of dimensions given as NULL, "x", "y", or "xy",
  # and outputs AICc table values and parameter info
  # and stuff like that.
  # Lists must have correct names (Maybe force a renams?)
  
  PDFs <- list(dexp,dweibull,dlnorm,dpareto,dgamma,
    function(x,xmid,b){
      if (x<=xmid){
        return (dexp(x,(b+1)/xmid))
      }
      return(dpareto(x,xmid,b))
    }
  )
  CDFs <- list(pexp,pweibull,plnorm,ppareto,pgamma,
    function(x,xmid,b){
      if (x<=xmid){
        return (pexp(x,(b+1)/xmid))
      }
      return(pexp(xmid,(b+1)/xmid)+ppareto(x,xmid,b))
    }
  )
  
  besties <- AICctab(fits)
  print(besties)
  
  colors = list("yellow","red","brown","blue","orange","green")
  
  names(CDFs)<-names(PDFs)<-names(colors)<-names(fits)
  # Or should I use names(besties) instead of names(fits)? 

  # designate best fit.
  bestfit = fits[min(as.numeric(besties))]
  # Use double brackets if you don't want the name
  
  ## CDF
  plot (xx,empFns.CDF(xx), log=logstring)
  for (name in names(CDFs)){
    if (length(coef(fits[[name]])) == 1){
      lines(xx,CDFs[[name]](xx,coef(fits[[name]])[1]), type="o", col=colors[[name]])
    }
    if (length(coef(fits[[name]])) == 2){
      lines(xx,CDFs[[name]](xx,coef(fits[[name]])[1] , coef(fits[[name]])[2]), type="o", col=colors[[name]])  
    }
  }
  if (length(coef(bestfit)) == 1){
    lines (xx,CDFs[[names(bestfit)]](xx,coef(bestfit)[1]),type="l",col=colors[[names(bestfit)]])
  }
  if (length(coef(bestfit)) == 2){
    lines (xx,CDFs[[names(bestfit)]](xx,coef(bestfit)[1] , coef(bestfit)[2]),type="l",col=colors[[names(bestfit)]])
  }
  
  ## PDF
  plot (xx,empFns.PDF(xx), log=logstring)  ### HOW DO I DO THIS HISTOGRAM STYLE???
  for (name in names(PDFs)){
    if (length(coef(fits[[name]])) == 1){
      lines(xx,PDFs[[name]](xx, coef(fits[[name]])[1]), type="o", col=colors[[name]])  
    }
    if (length(coef(fits[[name]])) == 2){
      lines(xx,PDFs[[name]](xx, coef(fits[[name]])[1], coef(fits[[name]])[2]), type="o", col=colors[[name]])  
    }
  }
  if (length(coef(bestfit)) == 1){
    lines (xx,PDFs[[names(bestfit)]](xx,coef(bestfit)[1]),type="l",col=colors[[names(bestfit)]])
  }
  if (length(coef(bestfit)) == 2){
    lines (xx,PDFs[[names(bestfit)]](xx,coef(bestfit)[1] ,coef(bestfit)[2] ),type="l",col=colors[[names(bestfit)]])
  }
      
  HazFun1<-function(x,pdf,cdf,theta){
    return (pdf(x,theta)/(1-cdf(x,theta)))
  }
  HazFun2<-function(x,pdf,cdf,theta){
    return (pdf(x,theta)/(1-cdf(x,theta[1],theta[2])))
  }
  ## HazFun
  plot (xx,empFns.Haz(xx), log=logstring)
  for (name in names(PDFs)){  
    if (length(coef(fits[[name]])) == 1){
      lines(xx,HazFun1(xx,PDFs[[name]],CDFs[[name]], theta), type="o", col=colors[[name]])  # might need to use brackets or double brackets
    }
    if (length(coef(fits[[name]])) == 2){
      lines(xx,HazFun2(xx,PDFs[[name]],CDFs[[name]], theta), type="o", col=colors[[name]])  # might need to use brackets or double brackets
    }
  }
  if (length(coef(bestfit)) == 1){
    lines (xx,HazFun1(xx,PDFs[[names(bestfit)]], CDFs[[names(bestfit)]] , theta),type="l",col=colors[[names(bestfit)]])
  }
  if (length(coef(bestfit)) == 2){
    lines (xx,HazFun2(xx,PDFs[[names(bestfit)]], CDFs[[names(bestfit)]] , theta),type="l",col=colors[[names(bestfit)]])
  }
}

  
