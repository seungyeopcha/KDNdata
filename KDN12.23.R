#패키지
library('ggplot2')
library(ggrepel)
library(gridExtra)
library(grid)
library(tidyr)
library(gtable)
library(dplyr)

#setwd
setwd("C:/data")

#데이터불러오기
df<-read.csv('KDNdata/개폐업.csv')

#평균+표준편차
sd<-apply(df[-1,-1],2,sd)
mean<-df[df$행정구=='서울시',-1]
mean<-as.data.frame(t(mean))

sd<-as.vector(sd)
mean$sd<-sd
colnames(mean)<-c('mean','sd')

#서울시제외
df1<-t(df[-1,])
a<-df[-1,]$행정구
colnames(df1)<-a
df1<-df1[-1,]

#합치기
df1<-as.data.frame(cbind(df1,mean))

#########계산
length(df1$mean)
df2<-data.frame(matrix(ncol = 25,nrow = 27))
colnames(df2)<-a
row.names(df2)<-분기

for(i in c(1:27)){
  m<-as.numeric(df1[i,26])
  a<-as.numeric(df1[i,27])
  k<-df1[i,c(1:25)]
  df2[i,]<-ifelse(k>=(m+2*a),2,
                  ifelse((m+a)<=k,1,
                         ifelse((m)<=k,-1,-2)))}

df2<-t(df2)

write.csv(df2,'KDNdata/개폐업점수.csv')






