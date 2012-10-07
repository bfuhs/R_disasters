# disasterPrep.r
# 
# This script contains the following functions for disaster data preparation
# 
# disPrep()
# Input: copied data
# Output: what I need
# usage example:
# something <- disprep(?)
#
# disSeries(x)
# Input: disPrep output
# Output: time series plots
# usage example:
# disSeries(?)
# 
# empSplines(x)
# Input: disPrep output
# Output: empirical cdf, pdf, and Haz splines
# usage example:
# something <- empSplines(?)
#
# maybe include:
# FYcostTotals<-c(189738690, 179029353, 170961105, 134433660, 139835580, 155433320, 251306098, 175792446, 157703365, 259592953, 290267133, 287778717, 489799468, 402754229, 368535030, 462608660, 599603129)
library(moments)  # for skew and kurtosis

disPrep <- function(){
  # First, highlight first 8 columns minus first row and copy
  
  disasters<-read.table("clipboard", header=F,sep="\t")
  
  realDates<-unlist(disasters[,5])
  realDates<-as.list(levels(realDates))[realDates]
  realDates=strptime(realDates,format="%m/%d/%y")
  realDates<-format.Date(realDates, "%Y-%m-%d")
  realDates = as.Date(realDates)
  
  #disasters<-as.matrix(disasters)
  #disasters<-data.matrix(disasters)
  #dimnames(disasters)=NULL

  disasters[,5] <- realDates
  
  disasters<-disasters[sort.list(disasters[,5]), ]
  
  realDates<-unlist(disasters[,5])
  #realDates<-as.list(levels(realDates))[realDates]
  #realDates=strptime(realDates,format="%m/%d/%y")
  #realDates<-format.Date(realDates, "%Y-%m-%d")
  #realDates<-as.Date(realDates)

  
  cost=disasters[,8]
  cost<-as.numeric(levels(cost))[cost]
  affected<-disasters[,6]
  affected<-as.numeric(levels(affected))[affected]
  dead<-disasters[,7]
  dead<-as.numeric(levels(dead))[dead]
  FY=disasters[,1]
  names(FY)<-NULL
  
  dataa <- cbind(FY, disasters[,2],disasters[,3],disasters[,4], disasters[,5], affected, dead, cost)
  
  intervalTimes<- disasters[-1,5] - disasters[-length(disasters[,5]),5]       

  dataa<-cbind(dataa,c(NA,intervalTimes))
  
  # Figure out how to convert date row back to date
  
  # possibly times<-Surv(as.numeric(x[],x[])) for interval?
  return (dataa)
}

disSeries <- function(dataa){
  # input will determine which disasters to plot.
  # Feed this columns 5-9 of disPrep output
  # Input should be time, affected, deaths, cost, time till onset
  # usage: 
  # 
  # Do a quad thing with everything a function of time
  # date=dataa[,1]
  # affected=dataa[,2]
  # deaths=dataa[,3]
  # cost=dataa[,4]
  # wait=dataa[,5]
  
  stats<-function(x){
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
  
  dates<-as.Date(dataa[,1], origin="1970-01-01")
  
  plot(dates,dataa[,2], log="y", xlab="Date", ylab="People affected")
  title(main="Number of people affected per disaster")
  print("Stats for Affected:")
  stats(dataa[,2])
  
  plot(dates,dataa[,3], log="y", xlab="Date", ylab="Deaths")
  title(main="Number of people killed per disaster")
  print("Stats for Deaths:")
  stats(dataa[,3])
  
  plot(dates,dataa[,4], log="y", xlab="Date", ylab="US Dollars")
  title(main="OFDA dollar response per disaster")
  print("Stats for Cost:")
  stats(dataa[,4])
  
  plot(dates,dataa[,5], log="y", xlab="Date", ylab="Days since last")
  title(main="Days since last disaster per disaster")
  print("Stats for Days Since Last:")
  stats(dataa[,5])
  

  # demo(graphics) for plotting stuff better?
}

# Do histograms too?

empSplines <- function(x){
  times<-Surv(as.numeric(x))
  
  
  return (0)
}

stationarityEpochs <- function (x, sets) {
  #  x = data vetor, sets = List of sets of indices
  # output is a list of means for the indices in each row
  IHateR <- function(indices){
    return (mean(x[indices], na.rm=TRUE))
  }
  return(lapply(sets,IHateR))
}

empiricalPlots <- function(timeDiffs){
  T <- sort(timeDiffs)
  cdf = rep(0,length(T))
  n=length(T)
  i<-1
  while(i<=length(T)) {
    cdf[i] <- i/n
    i<-i+1
  }
  
  plot(T,cdf)
  lines(T,cdf)
  # Should probably plot cumulative distribution function fit lines on this
  
  pdf = 0
  Tpdf = 0
  lowT = 0
  highT = 0
  cdfCount=1
  i=1
  lowi=1
  while (i<=length(T)){
    highT = T[i]
    if (lowT != highT){
      pdf = cbind(pdf, (cdf[i]-cdf[lowi]) / (T[i]-T[lowi]))
      Tpdf = cbind(Tpdf,ave(T[i],T[i-1]))
      lowT<-highT
      lowi<-i
    }
    i<-i+1
  }
  
  # I may want to exclude the zero - not sure

  # power= a*(b-1)/(x^b)
  
  k = 0.1905867
  b = 0.2196688
  x<-1:400
  weib = k*(x^(b-1))*exp(-k*(x^b)/b)
  
  plot(Tpdf,pdf)
  lines(Tpdf,pdf)
  lines(x, weib, type="l")
  
  plot(Tpdf,pdf, log="xy")  # Put the fits here
  lines(Tpdf,pdf)
  lines(x, weib, type="l")
}

