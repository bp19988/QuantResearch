## LESSON 1 ##

#Check for na values
any(is.na(df))
#check continuous (int) /discrete values (factors)
str(df)
#import libraries
library(tidyverse)
#Grab numeric columns only
num.cols <- sapply(df, is.numeric)
#filter numeric columns for correlations
cor.data <- cor(df[,num.cols])
#print
print(cor.data)
#packages
library(corrgram)
library(corrplot)
#corrplot
print(corrplot(cor.data, method = 'color'))
#plot histogram G3
ggplot(df, aes(x=G3)) + geom_histogram(bins = 20, alpha = .25, fill = "blue")

## LESSON 2 ##

# Split data into test and train set
#install caTools
library(caTools)
#set a seed (follows random seed set by lesson)
set.seed(101)
#split the sample
sample <- sample.split(df$G3, SplitRatio = 7/10)
#training set, 70% goes to train
train <- subset(df, sample==T)
# 30% to test
test <- subset(df, sample == F)

#general model (y is value to predict, x1 +x2 are predictors)
model <- lm(y ~ x1+x2,data)
model <- lm(y ~ ., data) #uses all features

#Train and build model
model <- lm(G3 ~., data = train)

#Interpret Model
#stars indicate the prob that the variable is not relavant, 
#smaller number, more relavant
print(summary(model))
#grabing residuals
res <- residuals(model)
res<-as.data.frame(res)
ggplot(res,aes(res)) + geom_histogram(fill = 'blue', alpha =.25, bins =100)
#find out that model predicts negative values for a test, not possible

## LESSON 3 ##

#predict
G3.Predict <- predict(model, test)

results <- cbind(G3.Predict, test$G3)
colnames(results) <- c("predicted","actual")
results <- as.data.frame(results)

#rm negative values
rm_neg <- function(x){
        if (x <0){
                return(0)
        }
        else{
                return(x)
        }
}

#apply function
results$predicted <- sapply(results$predicted, rm_neg)
#MSE (mean squared error)
mse <- mean((results$actual - results$predicted)^2)

hildog<-GET("https://api.maplight.org/maplight-api/fec/contributions", 
            query = list(candidate = "Hillary Clinton"))
hildog<-GET("https://api.maplight.org/maplight-api/fec/contributions", 
                        query = list(candidate_name = "Hillary Clinton"))
