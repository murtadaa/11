##install.packages("tseries")
library(tseries)

##data input
SandPIn <- get.hist.quote('^gspc',quote="Close")

##log data
SandPLog <- log(lag(SandPIn)) - log(SandPIn)

##Volatility measure 
SNPvol <- sd(SandPLog) * sqrt(250) * 100


##loop

Vol <- function(d, logrets)
{
  
  var = 0
  
  lam = 0
  
  varlist <- c()
  
  for (r in logrets) {
    
    lam = lam*(1 - 1/d) + 1
    
    var = (1 - 1/lam)*var + (1/lam)*r^2
    
    varlist <- c(varlist, var)
    
  }
  
  sqrt(varlist)
}


#Plotting  

volest <- Vol(10,SandPLog)
volest2 <- Vol(30,SandPLog)
volest3 <- Vol(100,SandPLog)

plot(volest,type="l")

lines(volest2,type="l",col="red")

lines(volest3, type = "l", col="blue")