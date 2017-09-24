library(tidyquant)
library(astsa)

SPX <- getSymbols("^GSPC",auto.assign = FALSE, from = "2015-01-01")
SPX <- dailyReturn(SPX)
SPXret <- SPX$GSPC.Close

SPXts <-as.ts(SPXret, start= c(2015, 1), frequency = 365)

acf2(SPXret, max.lag = 12)
sarima(SPXret, 0, 1, 1)
sarima.for(SPXret, n.ahead = 1, 0, 1, 1)

library(quantmod)
library(astsa)   

SPX <- getSymbols("^GSPC", auto.assign = FALSE, from = "2010-01-01")
SPXret <- SPX$GSPC.Close
SPXret <- ts(SPXret, frequency=365, start=c(2010,4))
sarima.for(SPXret, 5, 1, 1, 3)
