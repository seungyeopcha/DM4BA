# 기본설정 =====================================================================
getwd()
setwd("C:/data/R")

Amtrak.df<-read.csv("AmtrakBig_Raw.csv")
housing.df<-read.csv("HousingData.csv")
cereals.df<-read.csv("cereal.csv")
toyota.df<-read.csv("ToyotaCorolla.csv")

# library
library(ggplot2)
library(reshape)
#install.packages('ggmap')
library(ggmap)
#install.packages('forecast')
library(forecast)
#install.packages('gains')
library(gains)
options(scipen=999)
#=============================================================================== 
# set.seed
set.seed(0101)

# training and validation sets random generating
training<-sample(toyota.df$Id,600)
validation<-sample(setdiff(toyota.df$Id,training),400)

# linear regression model
reg<-lm(Price~.,data=toyota.df[,-c(1,2,8,11)],subset=training,na.action = na.exclude)

pred_t<-predict(reg,na.action=na.pass)

pred_v<-predict(reg,newdata = toyota.df[validation,-c(1,2,8,11)], na.action=na.pass)

# accuracy
## training
accuracy(pred_t,toyota.df[training,]$Price)

## validation
accuracy(pred_v,toyota.df[validation,]$Price)

# 향상차트와 십분위 향상차트
## library(gains)
gain<-gains(toyota.df[validation,]$Price[!is.na(pred_v)],pred_v[!is.na(pred_v)])
?gains

# 누적향상차트
price<-toyota.df[validation,]$Price[!is.na(toyota.df[validation,]$Price)]

plot(c(0,gain$cume.pct.of.total*sum(price))~c(0,gain$cume.obs),
     xlab='cases',ylab="Cumulative Price",main="Lift Chart",type="l",col="green")

# baseline
lines(c(0,sum(price))~c(0,dim(toyota.df[validation,])[1]),col="black",lty=2)

# 십분위 향상차트
barplot(gain$mean.resp/mean(price),names.arg = gain$depth,
        xlab="Percentile",ylab="Mean Response",main = "Decile-wise lift chart")










