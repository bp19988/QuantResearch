library(tidyquant)
library(astsa)
library(forecast)
library(tseries)
library(fGarch)

getSymbols("^GSPC", auto.assign = T, from = "2017-01-01")
SPXret <- GSPC$GSPC.Close
auto.arima(SPXret)
arimaSPX <- sarima(SPXret, 0, 1, 0)
sarima.for(SPXret, n.ahead = 4, 0, 1, 0)


arima.res <- test.arima$residuals
squared.res.arima <- arima.res^2

arima.010.garch.11 <-garch(arima.res, order=c(1,1), trace=F)

arima.010.garch.11 <- garchFit(formula = ~ garch(1,1), data = arima.res)

?garchSpec
?garchSim