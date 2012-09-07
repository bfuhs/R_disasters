# 
# disasterTest.r
# 7/26/12
# 
# functions: dataPrep, timeSeries, 
#    rawStats, empFunctions, fitAndPlot
# 
# fitinfo(x)
# takes data, returns list of mle2 fit objects
#

library(bbmle)
library(VGAM)

empFunctions <- function(x){
  cumuProb <- function(observations){
    n=length(observations)
    prob = rep(0,n)
    i<-1
    while(i<=n) {
      prob[i] <- i/n
      i<-i+1
    }
    return (prob)
  }
  CDFgen<- function(observations){
    cdf <-approxfun(x=observations, y=cumuProb(observations), method="constant", yleft=0, yright=1, rule=2, f=0, ties="max")
    return (cdf)
  }
  PDFgen<- function(observations){
    # x and y will be the points for linear interpolation
    y = 0
    x = 0
    low = observations[1]
    high = observations[2]
    n=length(observations)
    cProbs <- cumuProb(observations)
    i=2
    lowi=2
    while (i<=n){
      high = observations[i]
      if (low != high){
        y = cbind(y, (cProbs[i]-cProbs[lowi]) / (observations[i]-observations[lowi]))
        x = cbind(x,ave(observations[i],observations[i-1]))  ##NO OBS I-1!!  
        low<-high
        lowi<-i
      }
      i<-i+1
    }
    y=y[-1]
    x=x[-1]  
    pdf <- approxfun(x=x , y=y , method="linear", yleft=0, yright=0, rule=2, ties="max")
    return(pdf)
  }
  
  empiricalCDF<-CDFgen(x)
  empiricalPDF<-PDFgen(x)
  empiricalHaz<-function(observations){
    return (empiricalPDF(observations)/(1-empiricalCDF(observations)))
  }
  Fns = list(CDF=empiricalCDF, PDF=empiricalPDF, Haz=empiricalHaz)
  return(Fns)
}
  

fitAndPlot <- function(x, xx, logstring=NULL){  # Add other stuff (labels for plats)
  # input data (doesn't have to be sorted)
  # outputs data frame of maxed likelihood and parameters for all models
  # format: model# is row, pars vector, maxed lik, convergence?
  #
  # combine with......
  #
  # Can I suppress some magicLines prior to input or will it screw up the names?
  #plotStuff <- function(empFns,magicLines,xx,logstring=NULL){ #xx= MY DOMAIN!!!!!!
    # This function takes a list of empirical functions,
    # a list of fit objects, and plots one on top of the other
    # with logs of dimensions given as NULL, "x", "y", or "xy",
    # and outputs AICc table values and parameter info
    # and stuff like that.
    # Lists must have correct names (Maybe force a renams?)
    
    
  ## a bunch of loglikelihood functions
  # Let's see if there'a any of these hat can be simplified.
  # Or looked up and copied from the Internet
  
  xbar=mean(x)
  xmed=median(x)
  xsd=sd(x)
  n-length(x)
  hxmin=min(x)
  empFns<-empFunctions(x)
  
  
  # Exponential
  # H(x) = k : k>0
  # p(x) = k*exp(-k*x) : k>0
  # 1 parameter
  LogLExp <- function(k, x){
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
  LogLWeib <- function(a, b, x){
    n<-length(x)
    LogL <- n*log(a*(b^-a)) + (a-1)*sum(log(x)) - sum(x^a)/(b^a)
    return (-LogL)
  } 
  
  # Log-normal
  # H(x) = NOT normal(mu, sigma2). Something crazier/weirder
  # P(x) = something over x *exp(normal)
  # 2 parameters
  LogLLogNorm <- function(mu, sigma2, x){
    n=length(x)
    LogL <- n*log((2*pi*sigma2)^(-.5))-sum(log(x))-(1/(2*sigma2))*sum((log(x)-mu)^2)
    return(-LogL)
  }
  # What should I use for normal Hazard? Log-f(erf)?
  
  # Power Law
  # H(x) = b/x : # b>0, b<2??, x>=xmin
  # p(x) = b*(xmin^b)/(x^(b+1))  # b>0, b<2??, x>=xmin
  # One or two parameters
  LogLPower <- function(b, x){
    n<-length(x)
    LogL <- n*log(b*(min(x)^b)) - (b+1)*sum(log(x))
    return (-LogL) 
  }
  LogLPower2 <- function(b, cutoff, x){
    n<-length(x)
    LogL <- n*log(b*(cutoff^b)) - (b+1)*sum(log(x))
    return (-LogL)
  }
  # or should  do/say pareto?

  # Gamma, I think same as power law with exponential cut-off. MLE might be bimodal
  # H(x) = Look it up. It's crazy/weird.
  # P(x) = 1/(gamma(a)*b^a) * x^(a-1) * exp(-x/b) : b>0
  # 2 parameters
  LogLGamma<- function(a, b, x){
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
  LogLPartPower <- function (xmid, b ,x){
    xexp <- x[x<xmid]
    xpower <- x[x>=xmid]
    return ( LogLExp( (b+1)/xmid ,xexp) + LogLPower2(b, xmid,xpower) )
  } # Should I combine with Weib too?

  
  MLEguesses<- list( 	# These are known ML estimators and guesses for initial values
    xbar,							# MLE
    c(1,xbar),   						# using above; ORDER MAY CHANGE
    c( sum(log(x))/n, sum((log(x) - sum(log(x))/n)^2)/n), 	# MLE; muhat, sigma2hat
    (1+n/sum(log(x/hxmin))),		# xmin			# MLE	#Not anymore
    c( (xbar/xsd)^2, xsd^2/xbar),				# moments
    # ???							# moments??
    c(xmed, 1+n/sum(log(x[x>=xmed]/xmed))/2)		# guess
  )
  
    # WHAT??
  LBounds <- list(
    1e-4,
    c(1e-4,1e-4),
    c(-Inf,1e-4),
    1e-4, # xmin specified separately, but should be greater than 0
    c(1e-4,1e-4),
    # ???,
    c(1e-4, 1e-4)
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
  
  magicLines <- list(	# Might need some of this stuff to be lists
    mle2(LogLExp, start=list(k=MLEguesses$Exp), method="L-BFGS-B",data=list(x=x), optimizer="optim", lower=LBounds$Exp),
    mle2(LogLWeib, start=list(a=MLEguesses$Weib[1], b=MLEguesses$Weib[2]) , method="L-BFGS-B", optimizer="optim", data=list(x=x), lower=LBounds$Weib ),
    mle2(LogLLogNorm, start=list(mu=MLEguesses$LogNorm[1], sigma2=MLEguesses$LogNorm[2]) , method="L-BFGS-B", optimizer="optim", data=list(x=x), lower=LBounds$LogNorm ),
    mle2(LogLPower, start=list(b=MLEguesses$Pareto) , method="L-BFGS-B", optimizer="optim", data=list(x=x), lower=LBounds$Pareto),
    mle2(LogLGamma, start=list(a=MLEguesses$Gamma[1],b=MLEguesses$Gamma[2]) , method="L-BFGS-B", optimizer="optim", data=list(x=x), lower=LBounds$Gamma),
    mle2(LogLPartPower, start=list(xmid=MLEguesses$PartPower[1],b=MLEguesses$PartPower[2]) , method="L-BFGS-B", optimizer="optim", data=list(x=x), lower=LBounds$PartPower)
  )
  
  names(magicLines)<-names(MLEguesses)


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
  

  
  ## Takes a negative loglikelihood, length of data, number of parameters
  ## Returns AIC, AICc scores (Lower means better fit)
  #bestness <- function(loglikelihood, dataLength, parNum){
  #  AIC <- 2*parNum + 2*loglikelihood  # because LogLs are negative
  #  AICc <- AIC + (2*parNum*(parNum+1))/(dataLength-parNum-1)
  #  return (c(AIC, AICc))
  #}
  
  #besties=bestness() #BUT I CAN'T EVEN HACK magicLines APART!!!!
  #print(besties)
  
  #besties <-AICctab(magicLines)
  #print(besties) 
  bestiesraw<-lapply(magicLines, AIC)
  print(bestiesraw)
  
  
  colors = list("yellow","red","brown","blue","orange","green")
  
  names(CDFs)<-names(PDFs)<-names(colors)<-names(magicLines)
  # Or should I use names(besties) instead of names(magicLines)? 
  
#333333333333333333333333333333333
  
  # designate best fit.
  bestfit = magicLines[as.numeric(bestiesraw) == min(as.numeric(bestiesraw))]
  # Use double brackets if you don't want the name
  print(class(coef(magicLines$Exp)))
  print(coef(magicLines[[1]]))
  print(class(coef(magicLines[[1]])))
  print(length(coef(magicLines[[1]])))
  print(length(coef(magicLines[[2]])))
  print(coef(magicLines[[2]]))
  print(as.numeric(coef(magicLines$Exp)))
  print(class(coef(magicLines$Exp)))
  #print(coef(magicLines[[Weib]]))
  #print(coef(magicLines[[LogNorm]]))
  #print(coef(magicLines[[Pareto]]))
  #print(coef(magicLines[[Gamma]]))
  #print(coef(magicLines[[PartPower]]))
  

  
  ## CDF
  plot (xx,empFns$CDF(xx), log=logstring, type="s")
  for (name in names(CDFs)){
    if (length(coef(magicLines[[name]])) == 1){
      lines(xx,CDFs[[name]](xx,coef(magicLines[[name]])[1]), type="l", lty="dashed", col=colors[[name]])
    }
    if (length(coef(magicLines[[name]])) == 2){
      lines(xx,CDFs[[name]](xx,coef(magicLines[[name]])[1] , coef(magicLines[[name]])[2]), type="l", lty="dashed",col=colors[[name]])  
    }
  }
  if (length(coef(bestfit)) == 1){
    lines (xx,CDFs[[names(bestfit)]](xx,coef(bestfit)[1]),type="l",lwd=2,col=colors[[names(bestfit)]])
  }
  if (length(coef(bestfit)) == 2){
    lines (xx,CDFs[[names(bestfit)]](xx,coef(bestfit)[1] , coef(bestfit)[2]),type="l",lwd=2,col=colors[[names(bestfit)]])
  }
  
  ## PDF
  plot (xx,empFns$PDF(xx), log=logstring, type="h")  ### HOW DO I DO THIS HISTOGRAM STYLE???
  for (name in names(PDFs)){
    if (length(coef(magicLines[[name]])) == 1){
      lines(xx,PDFs[[name]](xx, coef(magicLines[[name]])[1]), type="l", lty="dashed",col=colors[[name]])  
    }
    if (length(coef(magicLines[[name]])) == 2){
      lines(xx,PDFs[[name]](xx, coef(magicLines[[name]])[1], coef(magicLines[[name]])[2]), type="l", lty="dashed",col=colors[[name]])  
    }
  }
  if (length(coef(bestfit)) == 1){
    lines (xx,PDFs[[names(bestfit)]](xx,coef(bestfit)[1]),type="l",lwd=2,col=colors[[names(bestfit)]])
  }
  if (length(coef(bestfit)) == 2){
    lines (xx,PDFs[[names(bestfit)]](xx,coef(bestfit)[1] ,coef(bestfit)[2] ),type="l",lwd=2,col=colors[[names(bestfit)]])
  }
      
  HazFun1<-function(x,pdf,cdf,theta){
    return (pdf(x,theta)/(1-cdf(x,theta)))
  }
  HazFun2<-function(x,pdf,cdf,theta){
    return (pdf(x,theta)/(1-cdf(x,theta[1],theta[2])))
  }
  ## HazFun
  plot (xx,empFns$Haz(xx), log=logstring, type="l")
  for (name in names(PDFs)){  
    if (length(coef(magicLines[[name]])) == 1){
      lines(xx,HazFun1(xx,PDFs[[name]],CDFs[[name]], theta), type="l", lty="dashed",col=colors[[name]])  # might need to use brackets or double brackets
    }
    if (length(coef(magicLines[[name]])) == 2){
      lines(xx,HazFun2(xx,PDFs[[name]],CDFs[[name]], theta), type="l", lty="dashed",col=colors[[name]])  # might need to use brackets or double brackets
    }
  }
  if (length(coef(bestfit)) == 1){
    lines (xx,HazFun1(xx,PDFs[[names(bestfit)]], CDFs[[names(bestfit)]] , theta),type="l",lwd=2, col=colors[[names(bestfit)]])
  }
  if (length(coef(bestfit)) == 2){
    lines (xx,HazFun2(xx,PDFs[[names(bestfit)]], CDFs[[names(bestfit)]] , theta),type="l",lwd=2,col=colors[[names(bestfit)]])
  }
}

  
