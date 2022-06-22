# 기본설정 =====================================================================
getwd()
setwd("C:/data/R")

Amtrak.df<-read.csv("AmtrakBig_Raw.csv")
housing.df<-read.csv("HousingData.csv")
cereals.df<-read.csv("cereal.csv")
toyota.df<-read.csv("ToyotaCorolla.csv")
mower.df<-read.csv("RidingMowers.csv")
delays_df<-read.csv("FlightDelays.csv")
bank_df<-read.csv("UniversalBank.csv")

## install.packages
#install.packages('ggmap')
#install.packages('forecast')
#install.packages('gains')
#install.packages('caret')
#install.packages('FNN')
#install.packages('rpart')
#install.packages('rpart.plot')

## library
library(ggplot2)
library(reshape)
library(ggmap)
library(forecast)
library(gains)
library(caret)
library(FNN)
library(e1071)
library(rpart)
library(rpart.plot)

options(scipen=999)

## set.seed
set.seed(0101)

## options
options(scipen = 999) # digits -> 소수점
#===============================================================================
# 승차식 잔디깎이 df 분류나무 모델
head(mower.df)
tree_class<-rpart(Ownership~.,data=mower.df,method='class', control = rpart.control(mxdepth=2))

# tree plot
prp(tree_class,type=1,extra=1,split.font = 1,varlen = -10)

# 개인 대출 수락 분류 나무모델
head(bank_df)

bank_df<-bank_df[,-c(1,5)]

# train and valid dataset 나누기
train_index<-sample(c(1:dim(bank_df)[1]),dim(bank_df)[1]*0.6)
train_df<-bank_df[train_index,]
valid_df<-bank_df[-train_index,]

# 대출 수락 분류 나무모델
ct_bank<-rpart(Personal.Loan~.,data = train_df,method="class")

## plot
prp(ct_bank,type=1,extra = 1,under=TRUE, split.font = 1,varlen = -10)

# 더 깊은 분류 나무모델
ct_bank_deeper<- rpart(Personal.Loan~.,data = train_df,method="class",cp=0,minsplit=1)

# 잎 노드 갯수 세기
length(ct_bank_deeper$frame$var[ct_bank_deeper$frame$var=="<leaf>"])

## plot
prp(ct_bank_deeper,type=1,extra = 1,under=TRUE, split.font = 1,varlen = -10)

# confusion matrix
pred_train<-predict(ct_bank,train_df,type="class")
confusionMatrix(pred_train,as.factor(train_df$Personal.Loan))
