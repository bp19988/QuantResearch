library(tidyquant)
library(astsa)
library(forecast)
library(tseries)
library(fGarch)

getSymbols("TXT", auto.assign = T, from = "2016-01-01")
TXT.ret <- TXT$TXT.Close
TXT.ret <- na.omit(TXT.ret)

auto.arima.txt <-auto.arima(TXT.ret)
fit.txt <- auto.arima.txt$arma[0:3]

arima.TXT <- arima(TXT.ret, fit.txt)

arima.TXT.forecast <-forecast(arima.TXT)
plot(arima.AAPL.forecast)

arima.res <- arima.TXT.forecast$residuals
squared.res.arima <- arima.res^2
squared.res.arima<- as.tibble(squared.res.arima)
squared.res.arima<- na.omit(squared.res.arima)

arima.garch <- garchFit(formula = ~ garch(1,1), data = squared.res.arima)

arima.garch.fitted <- fitted(arima.garch)
arima.garch.fitted<-as.tibble(arima.garch.fitted)

write.csv(arima.garch.fitted, "FittedGarchVal.csv")
#plot(predict(arima.garch.fitted$value))
plot(arima.TXT.forecast)

?garchSpec
?garchSim