##### Magnitudes.r
#
#
#

# Look at linear relationships between....
#   magnitudes
#   log of magnitudes (similar to straight linear)
#   log of one magnitude and not the other (exp relationship)
#   multiplication by another variable (like GDP of country)
#
#

# 1 Look at plotted variants of three relationships

# 2 Form Hypotheses

library(ggplot2)

#lm(cost ~ deaths*perCapGDP + affected*perCapGDP)
#lm(log(deaths) ~ affected)

# If I find a relationship, I need to 
#  Figure out what I want to track
#  Get a notion of uncertainty
#  Make a formula to convert anything to it

#> plot(CREDdata[,10],CREDdata[,11])
#> plot(CREDdata[,10],log(CREDdata[,11]))
#> plot(CREDdata[,11],log(CREDdata[,10]))
#> plot(log(CREDdata[,11]),log(CREDdata[,10]))
#> plot(log(CREDdata[,10]),log(CREDdata[,11]))
## What could possibly relate these?
#### Filter by Type? (Use GGplot to color) (see below)

#> plot(log(CREDdata[,10]),log(CREDdata[,12]))
#> plot(log(CREDdata[,11]),log(CREDdata[,12]))
### These seem to do slightly BETTER than previous relationship!

##p1 = ggplot(CREDdata, aes(x="Tot..Affected..", y="Killed.." , color="Type.." ))
#p1 = ggplot(CREDdata, aes(x=log(CREDdata[,11]), y=log(CREDdata[,10]), color=CREDdata[,7]))
#p1 = p1 + geom_point(size=2) + xlab("Affected") + ylab("Deaths")
#print(p1)

## With jitter
#p2 = ggplot(CREDdata, aes(x=log(CREDdata[,11]), y=log(CREDdata[,10]), color=CREDdata[,7]))
#p2 = p2 + geom_jitter(position=position_jitter(width = .35, height = .35)) + xlab("Affected") + ylab("Deaths")
#print(p2)

# Need to think about Sampling stuff

#p2 = ggplot(CREDdata, aes(x=log(CREDdata[,11]), y=log(CREDdata[,12]), color=CREDdata[,7]))
#p2 = p2 + geom_point() + xlab("Affected") + ylab("Deaths")
#print(p2)

#p4 = ggplot(CREDdata, aes( x=log(CREDdata[,11]), y=log(CREDdata[,10]), color=factor(CREDdata[,7])) )
#p4 = p4 + geom_point() + geom_smooth(method=lm) + xlab("log Affected") + ylab("log Deaths")
##method=lm can go in geom_smooth()  
#print (p4)

## THIS WORKS
##dataa = CREDdata[CREDdata[,11]>1000 & CREDdata[,10]>100,  ]
##print (dim(dataa))
#dataa = CREDdata
## or just dataa = CREDdata
#p4 = ggplot(dataa, aes( x=dataa[,11], y=dataa[,10], color=factor(dataa[,7])) )
#p4 = p4 + geom_point() + geom_smooth() + xlab("Affected") + ylab("Deaths")
#p4 = p4 + scale_x_log10() + scale_y_log10()
##method=lm can go in geom_smooth()  
#print (p4)

#for (disType in levels(factor(CREDdata[,7]))){
#  dataa = CREDdata[CREDdata[,7]==disType,]
#  p = ggplot(dataa, aes(x=log(dataa[,11]),  y=log(dataa[,10])))
#  p = p + geom_point() + geom_smooth(method=lm) + xlab("log Affected") + ylab("log Deaths") + ggtitle(disType)
#  print (p)
#} ### BREAKS because not enough of something or other

#length(CREDdata[ is.na(CREDdata[,11]) & is.na(CREDdata[,10]) & is.na(CREDdata[,12]),])
# ONLY 16 entries lack some kind of magnitude indicator

affected = sort( CREDdata[!is.na(CREDdata[,11]),11] )
deaths = sort( CREDdata[!is.na(CREDdata[,10]),10] )
cost = sort( CREDdata[!is.na(CREDdata[,12]),12] )
affectedHasDeaths <- sort( CREDdata[!is.na(CREDdata[,11]) & !is.na(CREDdata[,10]),11] )
affectedHasCost <- sort( CREDdata[!is.na(CREDdata[,11]) & !is.na(CREDdata[,12]),11] )
deathsHasAffected <- sort( CREDdata[!is.na(CREDdata[,10]) & !is.na(CREDdata[,11]),10] )
deathsHasCost <- sort( CREDdata[!is.na(CREDdata[,10]) & !is.na(CREDdata[,12]),10] )
costHasAffected <- sort( CREDdata[!is.na(CREDdata[,12]) & !is.na(CREDdata[,11]),12] )
costHasDeaths <- sort( CREDdata[!is.na(CREDdata[,12]) & !is.na(CREDdata[,10]),12] )

print ("Low p-val means Affected^Deaths not a random sample of Affected...")
print (ks.test( affectedHasDeaths, affected ))
print ("Low p-val means Affected^Cost not a random sample of Affected...")
print (ks.test( affectedHasCost, affected ))
print ("Low p-val means Deaths^Affected not a random sample of Deaths...")
print (ks.test( deathsHasAffected, deaths ))
print ("Low p-val means Deaths^Cost not a random sample of Deaths...")
print (ks.test( deathsHasCost, deaths ))
print ("Low p-val means Affected^Cost not a random sample of Cost...")
print (ks.test( costHasAffected, cost )) ###########
print ("Low p-val means Deaths^Cost not a random sample of Cost...")
print (ks.test( costHasDeaths, cost ))

#plot(density(affected), log="xy")
#lines(density(affectedHasDeaths), log="xy")
#lines(density(affectedHasCost), log="xy")
#plot(density(deaths), log="xy")
#lines(density(deathsHasAffected), log="xy")
#lines(density(deathsHasCost), log="xy")
#plot(density(cost), log="xy")
#lines(density(costHasAffected), log="xy")
#lines(density(costHasDeaths), log="xy")

## Do the actual power law thing on these

complementaryCDF <- function(x){
  return(1-ecdf(x)(x))
}

plot( affected, complementaryCDF(affected), log="xy", type="l", col="yellow", main="Affected" )
lines( affectedHasDeaths, complementaryCDF(affectedHasDeaths), col="orange" )
lines( affectedHasCost, complementaryCDF(affectedHasCost), col="green" )
legend(1,.01, legend = c("Affected", "Affected (has Deaths entry)", "Affected (has Cost entry)"), lty=1, col = c("yellow", "orange", "green") )

plot( deaths, complementaryCDF(deaths), log="xy", type="l", col="red", main="Deaths" )
lines( deathsHasAffected, complementaryCDF(deathsHasAffected), col="orange" )
lines( deathsHasCost, complementaryCDF(deathsHasCost), col="purple" )
legend(1,.01, legend = c("Deaths", "Deaths (has Affected entry)", "Deaths (has Cost entry)"), lty=1, col = c("red", "orange", "purple") )

plot( cost, complementaryCDF(cost), log="xy", type="l", col="blue", main="Cost" )
lines( costHasAffected, complementaryCDF(costHasAffected), col="green" )
lines( costHasDeaths, complementaryCDF(costHasDeaths), col="purple" )
legend(.01,.01, legend = c("Cost", "Cost (has Affected entry)", "Cost (has Deaths entry)"), lty=1, col = c("blue", "green", "purple") )

#d1 <- ggplot( data.frame() )
#d2 <- ggplot( data.frame() )
#d3 <- ggplot( data.frame() )
#print(d1)
#print(d2)
#print(d3)

