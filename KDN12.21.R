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

#1.임대료데이터 통합
##(1)데이터 불러오기
df1<-read.csv('KDNdata/임대료_(2013_1).csv')
df2<-read.csv('KDNdata/임대료_(2013_2).csv')
df3<-read.csv('KDNdata/임대료_(2013_3).csv')
df4<-read.csv('KDNdata/임대료_(2013_4).csv')
df5<-read.csv('KDNdata/임대료_(2014_1).csv')
df6<-read.csv('KDNdata/임대료_(2014_2).csv')
df7<-read.csv('KDNdata/임대료_(2014_3).csv')
df8<-read.csv('KDNdata/임대료_(2014_4).csv')
df9<-read.csv('KDNdata/임대료_(2015_1).csv')
df10<-read.csv('KDNdata/임대료_(2015_2).csv')
df11<-read.csv('KDNdata/임대료_(2015_3).csv')
df12<-read.csv('KDNdata/임대료_(2015_4).csv')
df13<-read.csv('KDNdata/임대료_(2016_1).csv')
df14<-read.csv('KDNdata/임대료_(2016_2).csv')
df15<-read.csv('KDNdata/임대료_(2016_3).csv')
df16<-read.csv('KDNdata/임대료_(2016_4).csv')
df17<-read.csv('KDNdata/임대료_(2017_1).csv')
df18<-read.csv('KDNdata/임대료_(2017_2).csv')
df19<-read.csv('KDNdata/임대료_(2017_3).csv')
df20<-read.csv('KDNdata/임대료_(2017_4).csv')
df21<-read.csv('KDNdata/임대료_(2018_1).csv')
df22<-read.csv('KDNdata/임대료_(2018_2).csv')
df23<-read.csv('KDNdata/임대료_(2018_3).csv')
df24<-read.csv('KDNdata/임대료_(2018_4).csv')
df25<-read.csv('KDNdata/임대료_(2019_1).csv')
df26<-read.csv('KDNdata/임대료_(2019_2).csv')
df27<-read.csv('KDNdata/임대료_(2019_3).csv')
df28<-read.csv('KDNdata/임대료_(2019_4).csv')
df29<-read.csv('KDNdata/임대료_(2020_1).csv')
df30<-read.csv('KDNdata/임대료_(2020_2).csv')
df31<-read.csv('KDNdata/임대료_(2020_3).csv')
df32<-read.csv('KDNdata/임대료_(2020_4).csv')
df33<-read.csv('KDNdata/임대료_(2021_1).csv')
df34<-read.csv('KDNdata/임대료_(2021_2).csv')
df16<-df16[,c(1:8)]

##(2) rowbind
###열이름 맞추기
names<-colnames(df1)
colnames(df1)<-names
colnames(df2)<-names
colnames(df3)<-names
colnames(df4)<-names
colnames(df5)<-names
colnames(df6)<-names
colnames(df7)<-names
colnames(df8)<-names
colnames(df9)<-names
colnames(df10)<-names
colnames(df11)<-names
colnames(df12)<-names
colnames(df13)<-names
colnames(df14)<-names
colnames(df15)<-names
colnames(df16)<-names
colnames(df17)<-names
colnames(df18)<-names
colnames(df19)<-names
colnames(df20)<-names
colnames(df21)<-names
colnames(df22)<-names
colnames(df23)<-names
colnames(df24)<-names
colnames(df25)<-names
colnames(df26)<-names
colnames(df27)<-names
colnames(df28)<-names
colnames(df29)<-names
colnames(df30)<-names
colnames(df31)<-names
colnames(df32)<-names
colnames(df33)<-names
colnames(df34)<-names

###rbind
df<-rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,df16,df17,df18,df19,df20,df21,df22,df23,df24,df25,df26,df27,df28,df29,df30,df31,df32,df33,df34)

##(3) 년도 분기 분리
df$년도<-substr(df$기간,1,4)
df$분기<-substr(df$기간,6,6)

df<-df[,-c(1,2)]

##(4)저장
write.csv(df,'KDN임대료.csv',row.names=F)

#2.전력데이터 통합
##(1)데이터 불러오기
df1<-read.csv('KDNdata/전력_(2013_1).csv');df1$년도<-2013;df1$분기<-1
df2<-read.csv('KDNdata/전력_(2013_2).csv');df2$년도<-2013;df2$분기<-2
df3<-read.csv('KDNdata/전력_(2013_3).csv');df3$년도<-2013;df3$분기<-3
df4<-read.csv('KDNdata/전력_(2013_4).csv');df4$년도<-2013;df4$분기<-4
df5<-read.csv('KDNdata/전력_(2014_1).csv');df5$년도<-2014;df5$분기<-1
df6<-read.csv('KDNdata/전력_(2014_2).csv');df6$년도<-2014;df6$분기<-2
df7<-read.csv('KDNdata/전력_(2014_3).csv');df7$년도<-2014;df7$분기<-3
df8<-read.csv('KDNdata/전력_(2014_4).csv');df8$년도<-2014;df8$분기<-4
df9<-read.csv('KDNdata/전력_(2015_1).csv');df9$년도<-2015;df9$분기<-1
df10<-read.csv('KDNdata/전력_(2015_2).csv');df10$년도<-2015;df10$분기<-2
df11<-read.csv('KDNdata/전력_(2015_3).csv');df11$년도<-2015;df11$분기<-3
df12<-read.csv('KDNdata/전력_(2015_4).csv');df12$년도<-2015;df12$분기<-4
df13<-read.csv('KDNdata/전력_(2016_1).csv');df13$년도<-2016;df13$분기<-1
df14<-read.csv('KDNdata/전력_(2016_2).csv');df14$년도<-2016;df14$분기<-2
df15<-read.csv('KDNdata/전력_(2016_3).csv');df15$년도<-2016;df15$분기<-3
df16<-read.csv('KDNdata/전력_(2016_4).csv');df16$년도<-2016;df16$분기<-4
df17<-read.csv('KDNdata/전력_(2017_1).csv');df17$년도<-2017;df17$분기<-1
df18<-read.csv('KDNdata/전력_(2017_2).csv');df18$년도<-2017;df18$분기<-2
df19<-read.csv('KDNdata/전력_(2017_3).csv');df19$년도<-2017;df19$분기<-3
df20<-read.csv('KDNdata/전력_(2017_4).csv');df20$년도<-2017;df20$분기<-4
df21<-read.csv('KDNdata/전력_(2018_1).csv');df21$년도<-2018;df21$분기<-1
df22<-read.csv('KDNdata/전력_(2018_2).csv');df22$년도<-2018;df22$분기<-2
df23<-read.csv('KDNdata/전력_(2018_3).csv');df23$년도<-2018;df23$분기<-3
df24<-read.csv('KDNdata/전력_(2018_4).csv');df24$년도<-2018;df24$분기<-4
df25<-read.csv('KDNdata/전력_(2019_1).csv');df25$년도<-2019;df25$분기<-1
df26<-read.csv('KDNdata/전력_(2019_2).csv');df26$년도<-2019;df26$분기<-2
df27<-read.csv('KDNdata/전력_(2019_3).csv');df27$년도<-2019;df27$분기<-3
df28<-read.csv('KDNdata/전력_(2019_4).csv');df28$년도<-2019;df28$분기<-4
df29<-read.csv('KDNdata/전력_(2020_1).csv');df29$년도<-2020;df29$분기<-1
df30<-read.csv('KDNdata/전력_(2020_2).csv');df30$년도<-2020;df30$분기<-2
df31<-read.csv('KDNdata/전력_(2020_3).csv');df31$년도<-2020;df31$분기<-3
df32<-read.csv('KDNdata/전력_(2020_4).csv');df32$년도<-2020;df32$분기<-4
df33<-read.csv('KDNdata/전력_(2021_1).csv');df33$년도<-2021;df33$분기<-1
df34<-read.csv('KDNdata/전력_(2021_2).csv');df34$년도<-2021;df34$분기<-2

##(2) rowbind
###열이름 맞추기
names<-colnames(df1)
colnames(df1)<-names
colnames(df2)<-names
colnames(df3)<-names
colnames(df4)<-names
colnames(df5)<-names
colnames(df6)<-names
colnames(df7)<-names
colnames(df8)<-names
colnames(df9)<-names
colnames(df10)<-names
colnames(df11)<-names
colnames(df12)<-names
colnames(df13)<-names
colnames(df14)<-names
colnames(df15)<-names
colnames(df16)<-names
colnames(df17)<-names
colnames(df18)<-names
colnames(df19)<-names
colnames(df20)<-names
colnames(df21)<-names
colnames(df22)<-names
colnames(df23)<-names
colnames(df24)<-names
colnames(df25)<-names
colnames(df26)<-names
colnames(df27)<-names
colnames(df28)<-names
colnames(df29)<-names
colnames(df30)<-names
colnames(df31)<-names
colnames(df32)<-names
colnames(df33)<-names
colnames(df34)<-names

###rbind
df<-rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,df16,df17,df18,df19,df20,df21,df22,df23,df24,df25,df26,df27,df28,df29,df30,df31,df32,df33,df34)
df<-df[,-1]

##(4)저장
write.csv(df,'KDN전력.csv',row.names=F)


#3. 임대료 전력데이터 통합
##(1) 데이터불러오기
dfa<-read.csv('KDN임대료.csv')
dfb<-read.csv('KDN전력.csv')
dfc<-read.csv('KDNdata/소속구.csv')

##(2) 임대료데이터에 소속구 merge
dfa<-merge(dfa,dfc,key=지역)

##(3) 임대료데이터에 전력데이터 merge
df<-merge(dfa,dfb,key=c(지역,년도,분기))

##(4) colname  변경
colnames(df)
names<-c("년도","분기","시군구","지역","임대료","공실률","투자수익률","소득수익률","자본수익률","전력추출정보","전력신설건","전력증설건","전력해지건")
colnames(df)<-names

##(5) 합계 제거
df<-df[df$전력추출정보!='합계',]
df<-df[df$지역!='소계',]
##(6) 저장
write.csv(df,'KDN통합.csv',row.names=F)
