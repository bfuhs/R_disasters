####
# disasterAnalyze.r
# Created 7/22/12 by Brendon Fuhs
# Updated 7/23/12 by Brendon Fuhs
#
### Contents:
#
## Analysis and likelihood -related functions are
# Model1 - Model4 pdfs
# Should I add a log-normal model? ???????????????????????????
# LogL, findMLE, bestness
#
## Empirical/non-parametric functions/estimates are
# cumuProb, empCDF, empPDF, Hazard
# (empCDF and empPDF return functions. Hazardfun only returns values)
#
## Display plots and stats -related functions are
# printStats, printFitInfo, plotTimeSeries
# plotDists
#


##################### Analysis, likelihood stuff

## Probability density functions

# H(t) = k : k>0
Model1 <- function(parameters, x){
  k <- parameters[1]
  n=length(x)
  if (k<=0){
    return (rep(0,n))
  }
  return (k*exp(-k*x))  # k>0
}

# H(t) = k : k>0
# p(t) = k*exp(-k*t) : k>0
LogL1 <- function(theta,y){
  k <- theta[1]
  n<-length(y)
  if (k<=0){
    return (Inf)
  }
  LogL <- n*log(k) - k*sum(y)
  return (-LogL)
}

# H(t) = a + b*t : a,b>0
Model2 <- function(parameters, x){
  a <- parameters[1]
  b <- parameters[2]
  n=length(x)
  if ((a<=0) || (b<=0)){
    return (rep(0,n))
  }
  return ((a + b*x)*exp(-x*(a + (b/2)*x))) # a,b>0
}

# H(t) = a + b*t : a,b>0
# p(t) = (a + b*t)*exp(-t*(a + (b/2)*t))
LogL2 <- function(theta,y){
  a <- theta[1]
  b <- theta[2]
  if ((a<=0) || (b<=0)){
    return (Inf)
  }
  LogL <- sum(log(a+b*y))-sum(y*(a+b*y/2))
  return (-LogL)
}

# H(t) = k*(t^(b-1)) : k>0, b>0(?)  THE BOOK MAY NOT BE CORRECT on p(t)
Model3 <- function(parameters, x){
  k <- parameters[1]
  b <- parameters[2]
  n=length(x)
  if ((k<=0) || (b<=0)){
    return (rep(0,n))
  }
  return (k*(x^(b-1))*exp(-k*(x^b)/b))  # k>0, b>0
}

# H(t) = k*(t^(b-1)) : k>0, b>0
# p(t) = k*(t^(b-1))*exp(-k*(t^b)/b) THE BOOK MAY NOT BE CORRECT
LogL3 <- function(theta,y){
  k <- theta[1]
  b <- theta[2]
  n<-length(y)
  if ((k<=0) || (b<=0)){
    return (Inf)
  }
  LogL <- n*log(k) + (b-1)*sum(log(y)) - (k/b)*sum(y^b)
  return (-LogL)
}

# H(t) = ??
# p(t) = b*(xmin^b)/(t^(b+1))  # b>0
# http://en.wikipedia.org/wiki/Pareto_distribution
Model4 <- function(parameters, x){
  xmin <- parameters[1]
  b <- parameters[2]
  n=length(x)
  for (h in x){
    if (b<=0 || h<xmin){
      return (rep(0,n))
    }
  }
  return (b*(xmin^b)/(x^(b+1)))  # b>0
}

# H(t) = ??
# p(t) = b*(tmin^b)/(t^(b+1))  # b>0
LogL4 <- function(theta,y){
  xmin <- theta[1]
  b <- theta[2]
  n<-length(y)
  for (h in y){
    if (b<=0 || h<xmin){
      return (Inf)
    }
  }
  LogL <- n*log(b*(xmin^b)) - (b+1)*sum(log(y))
  return (-LogL)
}

# Log-normal, mu and sigma2 > 0
Model5 <- function(parameters, x){
  mu <- parameters[1]
  sigma2 <- parameters[2]
  n=length(x)
  if (sigma2<=0){
    return (rep(0,n))
  }
  return ((1/(x*((2*sigma2*pi)^(.5))))*exp(-((log(x)-mu)^2) / (2*sigma2)))
}

# Log-normal
# http://en.wikipedia.org/wiki/Log-normal_distribution
LogL5 <- function(theta,y){
  mu <- theta[1]
  sigma2 <- theta[2]
  n=length(y)
  if (sigma2<=0){
    return (Inf)
  }
  LogL <- n*log((2*pi*sigma2)^(-.5))-sum(log(y))-(1/(2*sigma2))*sum((log(y)-mu)^2)
  return(-LogL)
}


### Generic negative loglikelihood function : x>0
#LogL <- function(parameters, Model, observations){
#  return (-sum(log(Model(parameters, observations))))   # Might need apply here?
#}
#LogLLogLLogLLogLLogLLogLLogL12341234
## takes initial parameters, model, and observations, and returns optim object
findMLE <- function (parameterGuess, LogL, observations){
  return (optim(parameterGuess, LogL, y=observations,method="SANN"))
  #return (nlminb(parameterGuess, LogL, y=observations))
}
#  method="L-BFGS-B", lower = rep(1e-5,length(parameterGuess)), upper =rep(1e+8,length(parameterGuess))

## Takes a negative loglikelihood, length of data, number of parameters
## Returns AIC, AICc scores (Lower means better fit)
bestness <- function(loglikelihood, dataLength, parNum){
  AIC <- 2*parNum + 2*loglikelihood  # because LogLs are negative
  AICc <- AIC + (2*parNum*(parNum+1))/(dataLength-parNum-1)
  return (c(AIC, AICc))
}

############################## Empirical/non-parametric functions/estimates

## This gets fed sorted data and is called by empCDF and empPDF
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

## Feed this sorted data
# It will output a cdf function
empCDF <- function(observations){
  cdf <-approxfun(x=observations, y=cumuProb(observations), method="constant", yleft=0, yright=1, rule=2, f=0, ties="max")
  return (cdf)
}

## Feed this sorted data
# It will output a pdf function
empPDF <- function(observations){
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

## Feed this the output of empcdf, output of pdf, and data.
Hazardfun <- function(pdf, cdf, observations){
  return (pdf(observations)/(1-cdf(observations)))
}

############################## Empirical stuff and plotting stuff

# Need to make this output prettier somehow.
printStats <- function(x){
  # Vital stats of vector x
  print("Mean = ")
  print(mean(x, na.rm=TRUE))
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
  print(sum(!is.na(x)))
  print(" ")
}

# Feed this sorted data?
printFitInfo <- function(observations){
  fit1 <- findMLE(1, LogL1, observations)
  fit2 <-findMLE(c(1,1), LogL2, observations)
  fit3 <-findMLE(c(1,1), LogL3, observations)
  fit4 <-findMLE(c(min(observations),0), LogL4, observations)
  fit5 <-findMLE(c(log(mean(observations)),log(1+(var(observations)/(mean(observations)^2)))), LogL5, observations)
  
  n=length(observations)
  #parNums = c(1,2,2,2,2)  #number of parameters in each model
  
  
      
#  print("Best parameters:")
#  print(bestfit[1:5]$par)
#  print("-LogL values")
#  print(bestfit[1:5]$values)
#  print("optim convergence flag:")
#  print(bestfit[1:5]$convergence)
#  print("AIC, AICc values")
  print("First:")
  print(fit1)
  print(bestness(fit1$value,n,1))
#  print(bestness(fit1$objective,n,1))
  print(" ")
  print("Second:")
  print(fit2)
  print(bestness(fit2$value,n,2))
#  print(bestness(fit2$objective,n,2))
  print(" ")
  print("Third:")
  print(fit3)
  print(bestness(fit3$value,n,2))
#  print(bestness(fit3$objective,n,2))
  print(" ")
  print("Fourth:")
  print(fit4)
  print(bestness(fit4$value,n,2))
#  print(bestness(fit4$objective,n,2))
  print(" ")
  print("Fifth:")
  print(fit5)
  print(bestness(fit5$value,n,2))
#  print(bestness(fit5$objective,n,2))
}


plotTimeSeries <- function(observations){
  
}

# Feed this sorted data
plotDists <- function(observations){
  fit1 <- findMLE(1, LogL1, observations)
  fit2 <-findMLE(c(1,1), LogL2, observations)
  fit3 <-findMLE(c(1,1), LogL3, observations)
  fit4 <-findMLE(c(min(observations),0), LogL4, observations)
  fit5 <-findMLE(c(log(mean(observations)),log(1+(var(observations)/(mean(observations)^2)))), LogL5, observations)
  
  CDF<-empCDF(observations)
  PDF<-empPDF(observations)
  x=seq(1,max(observations), by=max(observations)/1000)
  plot(observations, CDF(observations), log="xy")
  lines(x, CDF(x), type="l")
 #plot(observations, PDF(observations),log="y")
  plot(x, PDF(x), type="l",log="xy")
  lines(x, Model1(fit1$par, x), type="l", col="red")
  lines(x, Model2(fit2$par, x), type="l", col="yellow")
  lines(x, Model3(fit3$par, x), type="l", col="green")
  lines(x, Model4(fit4$par, x), type="l", col="blue")
  lines(x, Model5(fit5$par, x), type="l", col="purple")
  plot(x, Hazardfun(PDF,CDF,x), type="l", log="y")
  lines(x, Hazardfun(PDF,CDF,x))
}


FitsNGiggles <- function(x){
  # refers to empPDF, bestness
  print("Model1")
  fit1 <- optim(1, LogL1, y=x, method="L-BFGS-B", lower=10e-6)
  print(fit1)
  print(bestness(fit1$value,n,1))
  print(" ")
  print("Model2")
  fit2 <-optim(c(1,1), LogL2, y=x, method="L-BFGS-B", lower=c(10e-8,10e-8))
  print(fit2)
  print(bestness(fit2$value,n,2))
  print(" ")
  print("Model3")
  fit3 <-optim(c(1,1), LogL3, y=x, method="BFGS")
  print(fit3)
  print(bestness(fit3$value,n,2))
  print(" ")
  print("Model4")
  fit4 <-optim(c(min(x)/2,.5), LogL4, y=x, method="L-BFGS-B", lower=c(0,10e-6), upper=c(min(x),Inf))
  print(fit4)
  print(bestness(fit4$value,n,2))
  print(" ")
  print("Model5")
  fit5 <-optim(c(log(mean(x)),log(1+var(x)/(mean(x)^2))), LogL5, y=x, method="L-BFGS-B", lower=c(-Inf,0))
  print(fit5)
  print(bestness(fit5$value,n,2))
  print(" ")
  
  PDF<-empPDF(x)
  plot(x, PDF(x), type="l",log="xy")
  lines(x, Model1(fit1$par, x), type="l", col="red")
  lines(x, Model2(fit2$par, x), type="l", col="yellow")
  lines(x, Model3(fit3$par, x), type="l", col="green")
  lines(x, Model4(fit4$par, x), type="l", col="blue")
  lines(x, Model5(fit5$par, x), type="l", col="purple")
  
}



############################## Stationarity


############################## Data preparation stuff
# Maybe try to get alternative forms of input?

############################ Master Control do-it-all stuff

