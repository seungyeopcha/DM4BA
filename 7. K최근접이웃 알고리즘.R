# 기본설정 =====================================================================
getwd()
setwd("C:/data/R")

Amtrak.df<-read.csv("AmtrakBig_Raw.csv")
housing.df<-read.csv("HousingData.csv")
cereals.df<-read.csv("cereal.csv")
toyota.df<-read.csv("ToyotaCorolla.csv")
mower.df<-read.csv("RidingMowers.csv")

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

options(scipen=999)

## set.seed
set.seed(0101)

## options
options(scipen = 999) # digits -> 소수점
#=============================================================================== 
head(mower.df)

train_index<-sample(row.names(mower.df),0.6*dim(mower.df)[1])
valid_index<-setdiff(row.names(mower.df),train_index)
train_df<-mower.df[train_index,]
valid_df<-mower.df[valid_index,]

# new household
new_df<-data.frame(Income=60,Lot_Size=20)

# scatter plot
plot(Lot_Size~Income, data=train_df, pch=ifelse(train_df$Ownership=="Owner",1,3))
text(train_df$Income,train_df$Lot_Size,rownames(train_df),pos = 4)
text(60,20,"X")
legend("topright",c("owner","non-owner","newhousehold"),pch=c(1,3,4))

# standardization data use preProcess()
train_df.norm<-train_df
valid_df.norm<-valid_df
mower.df.norm<-mower.df

norm_values<-preProcess(train_df[,1:2],method=c('center','scale'))
train_df.norm[,1:2]<-predict(norm_values,train_df[,1:2])
valid_df.norm[,1:2]<-predict(norm_values,valid_df[,1:2])
mower.df.norm[,1:2]<-predict(norm_values,mower.df[,1:2])
new_df.norm<-predict(norm_values,new_df)

# knn() new_df
k_nearest<-knn(train = train_df.norm[,1:2],test=new_df.norm,cl=train_df.norm[,3],k=3)
row.names(train_df)[attr(k_nearest,"nn.index")]

## valid_df accuracy
knn.pred<-knn(train = train_df.norm[,1:2],test=valid_df.norm[,1:2],cl=train_df.norm[,3],k=3)
knn.pred
valid_df.norm[,3]
confusionMatrix(knn.pred,as.factor(valid_df.norm[,3]))

# accuracy by k values
accuracy_df<-data.frame(k=seq(1,14,1),accuracy=rep(0,14))

for(i in 1:14){
  knn.pred<-knn(train = train_df.norm[,1:2],test=valid_df.norm[,1:2],cl=train_df.norm[,3],k=i)
  accuracy_df[i,2]<-confusionMatrix(knn.pred,as.factor(valid_df.norm[,3]))$overall[1]
}

accuracy_df

# k=4 로 new_df 재분류
k_nearest<-knn(train = train_df.norm[,1:2],test=new_df.norm,cl=train_df.norm[,3],k=4)
row.names(train_df)[attr(k_nearest,"nn.index")]
k_nearest




