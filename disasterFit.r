# disasterFit.r
#
# function fitDisasters(t)
# Takes in a vector (t) of durations prior to (or between) onsets
#
# 
# calculates loglikelihoods according to the following models
# # (List models)
# 
# Finds max likelihood parameter fit for each model
# Displays fit lines for each model
# Calculates and displays AIC (BIC, LRT?) scores
# 
# Guides being used:
# http://www.unc.edu/~monogan/computing/r/MLE_in_R.pdf (some errors methinks)
# http://en.wikipedia.org/wiki/Akaike_information_criterion
#
# created and last updated 7/13/12

fitDisasters <- function(t){
  # use t to estimate PDF
  
  ## List negative loglikelihood functions of PDFs
  
  # H(t) = k : k>0
  # p(t) = k*exp(-k*t) : k>0
  LogLI <- function(theta,y){
    k <- theta[1]
    n<-length(y)
    LogL <- n*log(k) - k*sum(y)
    return (-LogL)
  }
  
  # H(t) = a + b*t : a,b>0
  # p(t) = (a + b*t)*exp(-t*(a + (b/2)*t))
  LogLII <- function(theta,y){
    a <- theta[1]
    b <- theta[2]
    LogL <- sum(log(a+b*y))-sum(y*(a+b*y/2))
    return (-LogL)
  }
  
  # H(t) = k*(t^(b-1)) : k>0, b>0
  # p(t) = k*(t^(b-1))*exp(-k*(t^b)/b) THE BOOK MAY NOT BE CORRECT
  LogLIII <- function(theta,y){
    k <- theta[1]
    b <- theta[2]
    n<-length(y)
    LogL <- n*log(k) + (b-1)*sum(log(y)) - (k/b)*sum(y^b)
      return (-LogL)
  }
  
  # H(t) = a + b/t + c*exp(t)
  # p(t) = (a + (b/y) + c*exp(y))*exp(-(a*y + b*y*(ln(y)-1) + c*(exp(y)-1)))
  # INTEGRAL DOESN'T CONVERGE Apparently. Double-check book
#  LogLIV <- function(theta,y){
#    a <- theta[1]
#    b <- theta[2]
 #   c <- theta[3]
 #   n<-length(y)
#    LogL <- sum(log(a+b/y+c*exp(y)))-a*sum(y)-b*sum(y*(log(y)-1))-c*sum(exp(y)-1)
#    return (-LogL)
#  }
  
  # H(t) = ??
  # p(t) = a*(b-1)/(t^b)  a>0, b>1
  LogLV <- function(theta,y){
    a <- theta[1]
    b <- theta[2]
    n<-length(y)
    LogL <- n*log(a*(b-1)) - b*sum(log(y))
    return (-LogL)
  }
  
  
  #t=c(2,4,6,8,10)
  t<-t[!is.na(t)]
  t<-t+(1e-10)
#  print(t)
  
#  print(LogLI(1,t))
#  print(LogLII(c(1,1),t))
#  print(LogLIII(c(1,1),t))
#  print(LogLIV(c(1,1,1),t))
  
  
  # optimize parameters for each loglikelihood function
# optimized <- c( optim(1, LogLI, y=t, method="L-BFGS-B"), optim(c(0,1), LogLII, y=t, method="BFGS"), optim(c(1,0), LogLIII, y=t, method="BFGS"), optim(c(0,1,1), LogLIV, y=t, method="BFGS") )
  # or...
  BestI <- optim(1, LogLI, y=t, method="L-BFGS-B", lower=1e-05, , upper = 1e10)
  print("Model I, parameters, LogL values, convergence flag")
  print(BestI$par)
  print(BestI$value)
  print(BestI$convergence)
  print(" ")
  
  BestII <-optim(c(1,1), LogLII, y=t, method="L-BFGS-B", lower=c(1e-05,1e-05), upper = c(1e+10,1e+10))
  print("Model II, parameters, LogL values, convergence flag")
  print(BestII$par)
  print(BestII$value)
  print(BestII$convergence)
  print(" ")
  
  BestIII <-optim(c(1,1), LogLIII, y=t, method="L-BFGS-B", lower=c(1e-05,1e-05), upper = c(1e+10,1e+10))
  print("Model III, parameters, LogL values, convergence flag")
  print(BestIII$par)
  print(BestIII$value)
  print(BestIII$convergence)
  print(" ")
  
#  BestIV <-optim(c(1,1,1), LogLIV, y=t, method="L-BFGS-B", lower=c(1e-05,1e-05,1e-05), upper = c(1e+10,1e+10,1e+10))
#  print("Model IV, parameters, LogL values, convergence flag")
#  print(BestIV$par)
#  print(BestIV$value)
#  print(BestIV$convergence)
#  print(" ")
  
  BestV <-optim(c(1,2), LogLV, y=t, method="L-BFGS-B", lower=c(1e-05, 1+(1e-05)), upper = c(1e+10,1e+10))
  print("Model V, parameters, LogL values, convergence flag")
  print(BestV$par)
  print(BestV$value)
  print(BestV$convergence)
  print(" ")
  
  
# optimized$pars
# optimized$value
  
  # Should probably check to make sure the optimization went okay here using built-in optim object stuff
  
  # calculate AIC score
  # might need to do something like https://stat.ethz.ch/pipermail/r-help/2006-June/106960.html
# k <- length(optimized$pars)
  n <- length(t)             # was nrow
  
  bestness <- function(likelihood,parNum, dataNum){
    AIC <- 2*parNum + 2*likelihood  # because LogLs are negative
    AICc <- AIC + (2*parNum*(parNum+1))/(dataNum-parNum-1)
    
    return (c(AIC, AICc))
  }
  
  print("Here are the AIC and AICc values. (Choose lowest AICc)")
  
  print(bestness(BestI$value,1,n))
  print(bestness(BestII$value,2,n))
  print(bestness(BestIII$value,2,n))
#  print(bestness(BestIV$value,3,n))
  print(bestness(BestV$value,2,n))
  
# AIC <- 2*k + 2*optimized$value  # because LogLs are negative
# AICc <- AIC + (2*k*(k+1))/(n-k-1)
  
  # Plot/show everything

#  AICc


}

