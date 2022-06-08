#패키지
library('ggplot2')
library(ggrepel)
library(gridExtra)
library(grid)
library(tidyr)
library(gtable)
library(dplyr)
library(corrplot)
options(scipen = 999)

#setwd
setwd("C:/data")

#전력데이터 점수 계산
df1<-read.csv('KDNdata/11일데이터/점포구성비_배당.csv')

#년도 분기별 평균, 표준편차
#2019_1
m1<-mean(df1[df1$연도==2019&df1$분기==1,'배당'])
a1<-sd(df1[df1$연도==2019&df1$분기==1,'배당'])

#2019_2
m2<-mean(df1[df1$연도==2019&df1$분기==2,'배당'])
a2<-sd(df1[df1$연도==2019&df1$분기==2,'배당'])

#2019_3
m3<-mean(df1[df1$연도==2019&df1$분기==3,'배당'])
a3<-sd(df1[df1$연도==2019&df1$분기==3,'배당'])

#2019_4
m4<-mean(df1[df1$연도==2019&df1$분기==4,'배당'])
a4<-sd(df1[df1$연도==2019&df1$분기==4,'배당'])

#2020_1
m5<-mean(df1[df1$연도==2020&df1$분기==1,'배당'])
a5<-sd(df1[df1$연도==2020&df1$분기==1,'배당'])

#2020_2
m6<-mean(df1[df1$연도==2020&df1$분기==2,'배당'])
a6<-sd(df1[df1$연도==2020&df1$분기==2,'배당'])

#2020_3
m7<-mean(df1[df1$연도==2020&df1$분기==3,'배당'])
a7<-sd(df1[df1$연도==2020&df1$분기==3,'배당'])

#2020_4
m8<-mean(df1[df1$연도==2020&df1$분기==4,'배당'])
a8<-sd(df1[df1$연도==2020&df1$분기==4,'배당'])

#2021_1
m9<-mean(df1[df1$연도==2021&df1$분기==1,'배당'])
a9<-sd(df1[df1$연도==2021&df1$분기==1,'배당'])

#2021_2
m10<-mean(df1[df1$연도==2021&df1$분기==2,'배당'])
a10<-sd(df1[df1$연도==2021&df1$분기==2,'배당'])

############################평균 표준편차
for(i in 1:length(df1$배당)){
  year<-as.numeric(df1$연도[i])
  k<-as.numeric(df1$분기[i])
  df1$평균[i]<-ifelse(year==2019&k==1,m1,
                    ifelse(year==2019&k==2,m2,
                           ifelse(year==2019&k==3,m3,
                                  ifelse(year==2019&k==4,m4,
                                        ifelse(year==2020&k==1,m5,
                                               ifelse(year==2020&k==2,m6,
                                                      ifelse(year==2020&k==3,m7,
                                                             ifelse(year==2020&k==4,m8,
                                                                    ifelse(year==2021&k==1,m9,m10)))))))))}

for(i in 1:length(df1$배당)){
  year<-as.numeric(df1$연도[i])
  k<-as.numeric(df1$분기[i])
  df1$표준편차[i]<-ifelse(year==2019&k==1,a1,
                    ifelse(year==2019&k==2,a2,
                           ifelse(year==2019&k==3,a3,
                                  ifelse(year==2019&k==4,a4,
                                         ifelse(year==2020&k==1,a5,
                                                ifelse(year==2020&k==2,a6,
                                                       ifelse(year==2020&k==3,a7,
                                                              ifelse(year==2020&k==4,a8,
                                                                     ifelse(year==2021&k==1,a9,a10)))))))))}
#######점수
for(i in 1:length(df1$배당)){
  m<-as.numeric(df1$평균[i])
  a<-as.numeric(df1$표준편차[i])
  k<-df1$배당[i]
  df1$점수[i]<-ifelse(k>=(m+2*a),2,
                  ifelse((m+a)<=k,1,
                         ifelse((m)<=k,-1,-2)))}

#######전력데이터 저장
write.csv(df1,"KDNdata/전력점수_최종.csv",row.names=F)

##########################점수데이터 통합
df1<-df1[,c('연도','분기','행정동','점수')]
df2<-read.csv('KDNdata/11일데이터/동별개폐업_점수.csv') #개폐업
df3<-read.csv('KDNdata/11일데이터/동별점포수_점수.csv') #프랜차이즈
df4<-read.csv('KDNdata/11일데이터/유동인구점수최종.csv') #유동인구
df5<-read.csv('KDNdata/11일데이터/매출점수최종.csv') #매출
df6<-read.csv('KDNdata/11일데이터/매출점수최종.csv') #매출

colnames(df1)[c(1,4)]<-c('년도','전력_점수')
colnames(df2)[10]<-'개폐업_점수'
colnames(df3)[10]<-'프랜차이즈_점수'
colnames(df5)[5]<-'매출_점수'

#점수열만
df1<-df1[c('년도','분기','행정동','전력_점수')]
df2<-df2[c('년도','분기','행정동','개폐업_점수')]
df3<-df3[c('년도','분기','행정동','프랜차이즈_점수')]
df4<-df4[c('년도','분기','행정동','유동인구_점수')]
df5<-df5[c('년도','분기','행정동','매출_점수')]

#매출데이터 제 제거
df5$행정동<-gsub('제','',df5$행정동)
df5$행정동<-gsub('기동','제기동',df5$행정동)
df5$행정동<-gsub('홍1동','홍제1동',df5$행정동)
df5$행정동<-gsub('홍2동','홍제2동',df5$행정동)
df5$행정동<-gsub('홍3동','홍제3동',df5$행정동)
df5$행정동<-gsub('홍4동','홍제4동',df5$행정동)

df6$행정동<-gsub('제','',df6$행정동)
df6$행정동<-gsub('기동','제기동',df6$행정동)
df6$행정동<-gsub('홍1동','홍제1동',df6$행정동)
df6$행정동<-gsub('홍2동','홍제2동',df6$행정동)
df6$행정동<-gsub('홍3동','홍제3동',df6$행정동)
df6$행정동<-gsub('홍4동','홍제4동',df6$행정동)


#merge
df<-merge(df1,df2,key=c('년도','분기','행정동'),all=T)
r1<-data.frame(table(df$행정동))
r1<-r1[r1$Freq!=10,]

df<-merge(df,df3,key=c('년도','분기','행정동'),all=T)
r2<-data.frame(table(df$행정동))
r2<-r2[r2$Freq!=10,]

df<-merge(df,df4,key=c('년도','분기','행정동'),all=T)
r3<-data.frame(table(df$행정동))
r3<-r3[r3$Freq!=10,]

df<-merge(df,df5,key=c('년도','분기','행정동'),all=T)
r4<-data.frame(table(df$행정동))
r4<-r4[r4$Freq!=10,]


#결측치 제거
df <- df[!is.na(df$전력_점수),]
df[is.na(df$개폐업_점수),'개폐업_점수']<--1
df[is.na(df$프랜차이즈_점수),'프랜차이즈_점수']<--1
df[is.na(df$유동인구_점수),'유동인구_점수']<--1
df[is.na(df$매출_점수),'매출_점수']<--1

#계산
df$젠트리피케이션_점수<-(0.329*df$개폐업_점수)+(0.3*df$유동인구_점수)+(0.05*df$전력_점수)+(0.089*df$프랜차이즈_점수)+(0.164*df$매출_점수)

#단계구분
for(i in 1:length(df$젠트리피케이션_점수)){
  k<-as.numeric(df$젠트리피케이션_점수[i])
  df$단계[i]<-ifelse(k>1,'위험',
                   ifelse(k>0,'경계',
                          ifelse(k>-1,'주의','초기')))}


#행정동 코드
dfa<-merge(df,df6[c('년도','분기','행정동코드','행정동')],key=c('년도','분기','행정동코드','행정동'),all.x=T)

#저장
write.csv(dfa,"젠트리피케이션단계_최종_2.csv",row.names=F)

