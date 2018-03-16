library(tidyquant)
library(astsa)
library(forecast)
library(tseries)
library(fGarch)

split.trained <-function(ret.vec){
  len <-length(ret.vec)
  smp_size <- floor(0.7 * len)
  
  ## set the seed to make your partition reproductible
  set.seed(123)
  data_ind <- sample(1:nrow(ret.vec), size = smp_size)
  
  train <- ret.vec[1:smp_size, ]
  test <- ret.vec[-(1:smp_size), ]
  train.test <- list(train, test)
  return(train.test)
  
}
MakeModels <- function(x){
  splitVec <-split.trained(x)
  arimaMod <-Arima.Model(splitVec[1])
  return(arimaMod)
}

Arima.Model <- function(x){
  arima.auto <-auto.arima(x)
  arima.fit <- auto.arima.txt$arma[0:3]

  arima.model <- arima(x, arima.fit)

  arima.forecast <-forecast(arima.model)


#plot(predict(arima.garch.fitted$value))

return(arima.forecast)
}
#takes in arima.forecast
# 
# Garch.Model <- function(x){
# arima.res <- x$residuals
# squared.res.arima <- arima.res^2
# squared.res.arima<- as.tibble(squared.res.arima)
# squared.res.arima<- na.omit(squared.res.arima)
#   arima.garch <- garchFit(formula = ~ garch(1,1), data = squared.res.arima)
#   
#   arima.garch.fitted <- fitted(arima.garch)
#   arima.garch.fitted<-as.tibble(arima.garch.fitted)
#   arima.garch.fitted <- na.omit(arima.garch.fitted)
#   write.csv(arima.garch.fitted, "FittedGarchVal.csv")
#   plot(predict(arima.garch.fitted$value))
# }
getSymbols("TXT", auto.assign = T, from = "2016-01-01")
TXT.ret <- TXT$TXT.Close
TXT.ret <- na.omit(TXT.ret)
y <-makeModels(TXT.ret)
plot(y)

?garchSpec
?garchSim
