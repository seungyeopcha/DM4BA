# 기본설정 =====================================================================
getwd()
setwd("C:/data/R")

Amtrak.df<-read.csv("AmtrakBig_Raw.csv")
housing.df<-read.csv("HousingData.csv")
cereals.df<-read.csv("cereal.csv")
toyota.df<-read.csv("ToyotaCorolla.csv")
mower.df<-read.csv("RidingMowers.csv")
delays_df<-read.csv("FlightDelays.csv")

## install.packages
#install.packages('ggmap')
#install.packages('forecast')
#install.packages('gains')
#install.packages('caret')
#install.packages('FNN')

## library
library(ggplot2)
library(reshape)
library(ggmap)
library(forecast)
library(gains)
library(caret)
library(FNN)
library(e1071)

options(scipen=999)

## set.seed
set.seed(0101)

## options
options(scipen = 999) # digits -> 소수점
#===============================================================================
head(delays_df)

# 수치형 변수 factor로 변경
delays_df$dayweek<-factor(delays_df$dayweek)
delays_df$deptime<-factor(delays_df$deptime)
delays_df$schedtime<-factor(round(delays_df$schedtime/100))

# 사용 변수 지정
selected.var<-c('dayweek','schedtime','origin','dest','carrier','delay')

# train, valid data 나누기
train_index<-sample(c(1:dim(delays_df)[1]),dim(delays_df)[1]*0.6)
train_df<-delays_df[train_index,selected.var]
valid_df<-delays_df[-train_index,selected.var]

# 나이브 베이즈
delays_nb<-naiveBayes(delay~.,data=train_df)
delays_nb

# 목적지 공항에 다른 운항상태의 피벗테이블
prop.table(table(train_df$delay,train_df$dest),margin=1)

# predict probabilities
pred.prob<-predict(delays_nb,newdata=valid_df,type="raw")

# predict class
pred.class<-predict(delays_nb,newdata=valid_df)

pred_df<-data.frame(actual=valid_df$delay,predicted=pred.class,pred.prob)

pred_df[valid_df$carrier=="DL"&valid_df$dayweek==7&valid_df$dest=="LGA"&valid_df$origin=="DCA",]
