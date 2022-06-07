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
##2016
df1<-read.table('KDNdata/매출2016/TBDM_BLCK_SELNG_QU_2016_1분기.txt',sep='|',header = T)
df2<-read.table('KDNdata/매출2016/TBDM_BLCK_SELNG_QU_2016_2분기.txt',sep='|',header = T)
df3<-read.table('KDNdata/매출2016/TBDM_BLCK_SELNG_QU_2016_3분기.txt',sep='|',header = T)
df4<-read.table('KDNdata/매출2016/TBDM_BLCK_SELNG_QU_2016_4분기.txt',sep='|',header = T)
##2017
df5<-read.table('KDNdata/매출2017/TBDM_BLCK_SELNG_QU_2017_1분기.txt',sep='|',header = T)
df6<-read.table('KDNdata/매출2017/TBDM_BLCK_SELNG_QU_2017_2분기.txt',sep='|',header = T)
df7<-read.table('KDNdata/매출2017/TBDM_BLCK_SELNG_QU_2017_3분기.txt',sep='|',header = T)
df8<-read.table('KDNdata/매출2017/TBDM_BLCK_SELNG_QU_2017_4분기.txt',sep='|',header = T)

df2016<-rbind(df1,df2,df3,df4)
df2017<-rbind(df5,df6,df7,df8)

df<-rbind(df2016,df2017)
options(scipen=999)

#행정동에 자치구 merge
a<-read.csv('KDNdata/자치구_행정동.csv')

dfa<-merge(df,a,key=ADSTRD_NM)

colnames(dfa)

dfa1<-dfa%>%
  group_by(STDR_YY_CODE_SE,STDR_QU_CODE_SE,자치구)%>%
  summarise(sum=sum(SUM_SELNG_AMT))

#점포수 
b<-read.csv('KDNdata/점포수_2016_2017.csv')

colnames(dfa1)<-c('년도','분기','자치구','매출')

b<-merge(b,dfa1,key=c(년도,분기,자치구))

b$매출<-as.numeric(b$매출)
b$점포수<-as.numeric(b$점포수)

b$매출나누기점포수<-as.numeric(b$매출)/as.numeric(b$점포수)

#평균 표준편차
b1<-b%>%
  group_by(년도,분기)%>%
  summarise(평균=mean(매출나누기점포수),표준편차=sd(매출나누기점포수))

b<-merge(b,b1,key=c(년도,분기))

r<-c(1:200)
for(i in c(1:200)){
  m<-b$평균[i]
  a<-b$표준편차[i]
  k<-b$매출나누기점포수[i]
  r[i]<-ifelse(k>=(m+2*a),2,
                  ifelse((m+a)<=k,1,
                         ifelse((m)<=k,-1,-2)))}
b$점수<-r

write.csv(b,"매출점수.csv",row.names=F)
