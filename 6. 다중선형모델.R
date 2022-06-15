# 기본설정 =====================================================================
getwd()
setwd("C:/data/R")

Amtrak.df<-read.csv("AmtrakBig_Raw.csv")
housing.df<-read.csv("HousingData.csv")
cereals.df<-read.csv("cereal.csv")
toyota.df<-read.csv("ToyotaCorolla.csv")

## library
library(ggplot2)
library(reshape)
#install.packages('ggmap')
library(ggmap)
#install.packages('forecast')
library(forecast)
#install.packages('gains')
library(gains)
options(scipen=999)

## set.seed
set.seed(0101)

## options
options(scipen = 999) # digits -> 소수점
#=============================================================================== 
# select variables for lm
selected.var<-c(3,4,7,8,9,10,12,13,14,17,18)

# data partition
train_idx<-sample(c(1:nrow(toyota.df)),0.6*nrow(toyota.df))
train_df<-toyota.df[train_idx,selected.var]
test_df<-toyota.df[-train_idx,selected.var]

# use lm()
toyoyta.lm<-lm(Price~.,data=train_df)

summary(toyoyta.lm)

# predict
toyota.lm.pred<-predict(toyoyta.lm,test_df)
resd<-test_df$Price-toyota.lm.pred

df.resd<-data.frame("Actual"=test_df$Price,"Predicted"=toyota.lm.pred,"Residuals"=resd)
df.resd

accuracy(toyota.lm.pred,test_df$Price)

# hist
hist(resd,breaks=30,,xlab="Residuals",main="")
axis(side=1,at=seq(-10000,5000,2000))

# use step() in leaps
# "backward", "forward", "both"
## 후방소거법
car.lm.step<-step(toyoyta.lm,direction="backward")
summary(car.lm.step) 
car.lm.step.pred<-predict(car.lm.step,test_df)
accuracy(car.lm.step.pred,test_df$Price)

## 전진선택법
car.lm.step<-step(toyoyta.lm,direction="forward")
summary(car.lm.step) 
car.lm.step.pred<-predict(car.lm.step,test_df)
accuracy(car.lm.step.pred,test_df$Price)

## 단계선택법
car.lm.step<-step(toyoyta.lm,direction="both")
summary(car.lm.step) 
car.lm.step.pred<-predict(car.lm.step,test_df)
accuracy(car.lm.step.pred,test_df$Price)

# use regsubsets() 
# install.packages('leaps')
library(leaps)

# create dummies for fuel type
Fuel_Type<-as.data.frame(model.matrix(~0+Fuel_Type,data = train_df))

# replace fuel_type col with 2 dummies
train_df<-cbind(train_df[,-4],Fuel_Type[,])
head(train_df)

search<-regsubsets(Price~.,data=train_df,nbest=1,nvmax=dim(train_df)[2],
                   method = "exhaustive")
sum<-summary(search)

# show model
sum$which

# show metrics
sum$rsq
sum$adjr2
sum$cp
