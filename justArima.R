library(astsa)
library(forecast)
library(quantmod)
library(lattice)
library(timeSeries)
library(rugarch)
getSymbols("^GSPC", auto.assign = T, from = "2017-01-01")

SPXret <- GSPC$GSPC.Close

#SPXts <- as.ts(SPXret)

acf2(SPXret, max.lag = 12)
auto.arima(SPXret)

sarima(SPXret, 0, 1, 0)
sarima.for(SPXret, n.ahead = 4, 0, 1, 0)

SPX <- getSymbols("^GSPC", auto.assign = FALSE, from = "2010-01-01")
SPXret <- SPX$GSPC.Close
SPXret <- ts(SPXret)
sarima.for(SPXret, 5, 1, 1, 3)