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

#은민 전력 가정x주택 통합
df1<-read.csv('KDNdata/주택용X가정용/신설+증설_면적.csv')
df2<-read.csv('KDNdata/주택용X가정용/신설+증설_직전분기 실사용량.csv')
df3<-read.csv('KDNdata/주택용X가정용/신설+증설_면적당사용량.csv')

df4<-read.csv('KDNdata/주택용X가정용/해지_면적.csv')
df5<-read.csv('KDNdata/주택용X가정용/해지_직전분기 실사용량.csv')
df6<-read.csv('KDNdata/주택용X가정용/해지_면적당사용량.csv')

df7<-read.csv('KDNdata/주택용X가정용/신설+증설+해지_면적.csv')
df8<-read.csv('KDNdata/주택용X가정용/신설+증설+해지_직전분기 실사용량.csv')
df9<-read.csv('KDNdata/주택용X가정용/신설+증설+해지_면적당사용량.csv')

df10<-read.csv('KDNdata/주택용X가정용/신설+증설-해지_면적.csv')
df11<-read.csv('KDNdata/주택용X가정용/신설+증설-해지_직전분기 실사용량.csv')
df12<-read.csv('KDNdata/주택용X가정용/신설+증설-해지_면적당사용량.csv')

#은정 전력 사업자.순수서비스X나머지_전력
dfb<-read.csv('KDNdata/사업자.순수서비스X나머지_전력.csv')

#컬럼이름 변경
colnames(df1)[4]<-'ef13'
colnames(df2)[4]<-'ef14'
colnames(df3)[4]<-'ef15'
colnames(df4)[4]<-'ef16'
colnames(df5)[4]<-'ef17'
colnames(df6)[4]<-'ef18'
colnames(df7)[4]<-'ef19'
colnames(df8)[4]<-'ef20'
colnames(df9)[4]<-'ef21'
colnames(df10)[4]<-'ef22'
colnames(df11)[4]<-'ef23'
colnames(df12)[4]<-'ef24'

#통합
df<-merge(df1,df2,key=c(년도,분기,자치구))
df<-merge(df,df3,key=c(년도,분기,자치구))
df<-merge(df,df4,key=c(년도,분기,자치구))
df<-merge(df,df5,key=c(년도,분기,자치구))
df<-merge(df,df6,key=c(년도,분기,자치구))
df<-merge(df,df7,key=c(년도,분기,자치구))
df<-merge(df,df8,key=c(년도,분기,자치구))
df<-merge(df,df9,key=c(년도,분기,자치구))
df<-merge(df,df10,key=c(년도,분기,자치구))
df<-merge(df,df11,key=c(년도,분기,자치구))
df<-merge(df,df12,key=c(년도,분기,자치구))

df_final<-merge(dfb,df,key=c(년도,분기,자치구))


#################################################
#젠트리 점수계산
df11<-read.csv('KDNdata/젠트리지수변수1617/영업기간&상주인구 점수.csv')
df12<-read.csv('KDNdata/젠트리지수변수1617/프랜차이즈&개폐업 점수.csv')
df13<-read.csv('KDNdata/젠트리지수변수1617/매출점수.csv')
df14<-read.csv('KDNdata/젠트리지수변수1617/유동인구 점수.csv')

#통합
dfa<-merge(df11,df12,key=c(년도,분기,자치구))
dfa<-merge(dfa,df13,key=c(년도,분기,자치구))
dfa<-merge(dfa,df14,key=c(년도,분기,자치구))

#계산
dfa$젠트리피케이션_점수<-(0.134*dfa$상주인구_점수)+(0.202*dfa$유동인구_점수)+(0.141*dfa$개폐업_점수)+
  (0.134*dfa$영업기간_점수)+(0.150*dfa$프랜차이즈_점수)+(0.239*dfa$매출_점수)

###############총통합
df_final<-merge(df_final,dfa[,c(1,2,3,10)],key=c(년도,분기,자치구))

########################상관분석
m_df<-df_final[,-c(1,2,3)]
mcor<-cor(m_df)
corrplot(mcor, method='color', shade.col=NA, tl.col='black', tl.srt=45)

corrplot(mcor, method='number', shade.col=NA, tl.col='black', tl.srt=45)

mcor_df<-data.frame(mcor)

write.csv(mcor_df,'젠트리피케이션상관계수.csv')


