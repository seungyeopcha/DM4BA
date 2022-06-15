# 기본설정 =====================================================================
getwd()
setwd("C:/data/R")

Amtrak.df<-read.csv("AmtrakBig_Raw.csv")
housing.df<-read.csv("HousingData.csv")
cereals.df<-read.csv("cereal.csv")

# library
library(ggplot2)
library(reshape)
#install.packages('ggmap')
library(ggmap)

#=============================================================================== 

# 요약통계량
head(housing.df,9) # head
summary(housing.df) # summary

# 단일 col 통계량
mean(housing.df$CRIM,na.rm = T)
sd(housing.df$CRIM,na.rm = T)
min(housing.df$CRIM,na.rm = T);min(housing.df$CRIM)
max(housing.df$CRIM,na.rm = T);max(housing.df$CRIM)
median(housing.df$CRIM,na.rm = T);median(housing.df$CRIM)
length(housing.df$CRIM)

# NA 수 찾기
sum(is.na(housing.df$CRIM));sum(is.na(housing.df))


# 요약통계량2
df.stat<-data.frame(평균=sapply(housing.df,function(x) mean(x, na.rm=TRUE)),
           표준편차=sapply(housing.df,function(x) sd(x, na.rm=TRUE)),
           최대=sapply(housing.df,function(x) max(x, na.rm=TRUE)),
           최소=sapply(housing.df, function(x) min(x, na.rm=TRUE)),
           중앙값=sapply(housing.df,function(x) median(x, na.rm=TRUE)),
           length=sapply(housing.df,length),
           na.cnt=sapply(housing.df,function(x) sum(length(which(is.na(x)))))
           )
df.stat

# 상관관계표
round(cor(housing.df,use='complete.obs'),3)

# heatmap with value
# heatmap(round(cor(housing.df,use='complete.obs'),3),Rowv = NA,Colv = NA)
cor.mat<-round(cor(housing.df,use='complete.obs'),3)
# cor.mat
cor.mat.melt<-melt(cor.mat)
# cor.mat.melt

ggplot(cor.mat.melt,aes(x=X1,y=X2,fill=value))+
  geom_tile()+
  geom_text(aes(x=X1,y=X2,label=value))

# 피벗테이블
table(housing.df$CHAS)

# Data binning =================================================================
## .bincode() in R
?.bincode
# x= value, b = 절단점
x <- c(0, 0.01, 0.5, 0.99, 1);b <- c(-0.001, 0.5, 0.9, 3)

.bincode(x, b, TRUE)

.bincode(x, b, FALSE)

.bincode(x, b, TRUE, TRUE)

.bincode(x, b, FALSE, TRUE)
# ==============================================================================

# create bins of size 1
housing.df$RM.bin<-.bincode(housing.df$RM, c(1:9))

housing.df$RM.bin

aggregate(housing.df$MEDV,by=list(RM=housing.df$RM.bin,CHAS=housing.df$CHAS),FUN=mean)

# 피벗테이블 with melt(), cast()
## 1. read.csv
## 2. create bins
housing.df$RM.bin<-.bincode(housing.df$RM, c(1:9))

## 3.  melt()
?melt
mlt<-melt(housing.df,id=c('RM.bin','CHAS'),measure=c("MEDV"))
head(mlt,5)

## 4. cast()
?cast
cast(mlt,RM.bin~CHAS, subset=variable=="MEDV",margins=c("grand_row","grand_col"),mean)

# create cat.MEDV===============================================================
## MEDV가 30이상이면 1 아니면 0
housing.df$MEDV

housing.df$CAT.MEDV<-ifelse(housing.df$MEDV>=30,1,0)

housing.df$CAT.MEDV
# ==============================================================================
# 범주형 변수의 범주 개수 축소
tbl<-table(housing.df$CAT.MEDV,housing.df$ZN)
tbl
prop.tbl<-prop.table(tbl,margin=2)
prop.tbl
?prop.table

barplot(prop.tbl,xlab = "ZN",ylab = "",yaxt="n",main="Distribution of CAT.MEDV by ZN")
axis(2,at=(seq(0,1,0.2)),paste(seq(0,100,20),"%"))

# ==============================================================================
# 차원축소 PCA
# ==============================================================================
## 1.
pcs<-prcomp(data.frame(cereals.df$calories,cereals.df$rating))

pcs
summary(pcs)
pcs$rot # 회전행렬
scores<-pcs$x

head(scores,5)

## 2.
pcs<-prcomp(na.omit(cereals.df[,-c(1:3)]))

summary(pcs)
pcs$rot[,1:5]

## 3. scale
pcs.cor<-prcomp(na.omit(cereals.df[,-c(1:3)]),scale.=T)
summary(pcs.cor)
pcs.cor$rot[,1:5]





