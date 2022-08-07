# 기본설정 =====================================================================
getwd()
setwd("C:/data/R")

bread<-read.csv("BreadBasket_DMS.csv", header = T)

## install.packages
#install.packages('ggmap')
#install.packages('forecast')
#install.packages('gains')
#install.packages('caret')
#install.packages('FNN')
#install.packages('rpart')
#install.packages('rpart.plot')
install.packages('arules')


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
library(arules)

## set.seed
set.seed(0101)

## options
options(scipen = 999) # digits -> 소수점

#===============================================================================

trans<-read.transactions("BreadBasket_DMS.csv", format="single", cols=c(3,4), sep=",", rm.duplicates=T)

rules<-apriori(fp.trans,parameter = list(supp = o.2, conf = o.5 , target = "rules"))

inspect(head(sort(rules, by= "lift"), n=6))
















