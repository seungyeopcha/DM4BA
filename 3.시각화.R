# 기본설정
getwd()
setwd("C:/data/R")

Amtrak.df<-read.csv("AmtrakBig_Raw.csv")
housing.df<-read.csv("HousingData.csv")

# NA 히트맵
heatmap(1*is.na(housing.df),Rowv=NA,Colv=NA,scale = "none", cexCol = 0.8)

# 산점도 matrix
plot(housing.df[,c(1,3,13,14)])

# 산점도, 분포, 상관계수 matrix
install.packages('GGally')
library(GGally)

ggpairs(housing.df[,c(1,3,13,14)])
