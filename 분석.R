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

#데이터
df<-read.csv('KDN모델링데이터셋.csv')
df<-df[!(df$년도==2019&df$분기==1),]
df<-df[,4:9]
df1<-read.csv("KDN모델링데이터셋마1.csv")
df1<-df1[!(df1$년도==2019&df1$분기==1),]
df1<-df1[,4:9]

df$단계<-factor(df$단계)
levels(df$단계)
df1$단계<-factor(df1$단계)
levels(df1$단계)


#랜덤포레스트----------------------------------
library(randomForest)
library(caret)
set.seed(0101)

#####----------------#1111111. 결측치 0----------------------------
a<-df[df$단계=="경계",]
b<-df[df$단계=="위험",]
c<-df[df$단계=="주의",]
d<-df[df$단계=="초기",]


##train data set
idx1<-sample(1:nrow(df[df$단계=="경계",]),nrow(df[df$단계=="경계",])*0.7)
idx2<-sample(1:nrow(df[df$단계=="위험",]),nrow(df[df$단계=="위험",])*0.7)
idx3<-sample(1:nrow(df[df$단계=="주의",]),nrow(df[df$단계=="주의",])*0.7)
idx4<-sample(1:nrow(df[df$단계=="초기",]),nrow(df[df$단계=="초기",])*0.7)


train1<-a[idx1,] ;test1<-a[-idx1,]
train2<-b[idx2,] ;test2<-b[-idx2,]
train3<-c[idx3,] ;test3<-c[-idx3,]
train4<-d[idx4,] ;test4<-d[-idx4,]

train<-rbind(train1,train2,train3,train4)
test<-rbind(test1,test2,test3,test4)

#모델생성
##Error in `[.default`(xj, i) : invalid subscript type 'list' => subset에는 벡터값만
##Error in randomForest.default(m, y, ...) : Can't have empty classes in y. ==> 팩터 레벨중 비어있는 값이 있을 때 
model<-randomForest(단계~.,data=train,importance=T)
model

#traindata로 모델 예측값출력
confusionMatrix(predict(model),train$단계)

#model을 활용한 test데이터 예측
pred<-predict(model,newdata=test)
confusionMatrix(pred,test$단계)

#변수들 중요도 플롯팅
varImpPlot(model)

#####----------------#1111111. 결측치 -1----------------------------
a<-df1[df1$단계=="경계",]
b<-df1[df1$단계=="위험",]
c<-df1[df1$단계=="주의",]
d<-df1[df1$단계=="초기",]


##train data set
idx1<-sample(1:nrow(df1[df1$단계=="경계",]),nrow(df1[df1$단계=="경계",])*0.7)
idx2<-sample(1:nrow(df1[df1$단계=="위험",]),nrow(df1[df1$단계=="위험",])*0.7)
idx3<-sample(1:nrow(df1[df1$단계=="주의",]),nrow(df1[df1$단계=="주의",])*0.7)
idx4<-sample(1:nrow(df1[df1$단계=="초기",]),nrow(df1[df1$단계=="초기",])*0.7)


train1<-a[idx1,] ;test1<-a[-idx1,]
train2<-b[idx2,] ;test2<-b[-idx2,]
train3<-c[idx3,] ;test3<-c[-idx3,]
train4<-d[idx4,] ;test4<-d[-idx4,]

train<-rbind(train1,train2,train3,train4)
test<-rbind(test1,test2,test3,test4)
#모델생성
##Error in `[.default`(xj, i) : invalid subscript type 'list' => subset에는 벡터값만
##Error in randomForest.default(m, y, ...) : Can't have empty classes in y. ==> 팩터 레벨중 비어있는 값이 있을 때 
model<-randomForest(단계~.,data=train,importance=T)
model

#traindata로 모델 예측값출력
confusionMatrix(predict(model),train$단계)

#model을 활용한 test데이터 예측
pred<-predict(model,newdata=test)
confusionMatrix(pred,test$단계)

#변수들 중요도 플롯팅
varImpPlot(model)

#===========인공 신경망 ========================================================================
## 신경망 라이브러리
install.packages("nnet")
library(nnet)

library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
install.packages("neuralnet")
library(neuralnet)
library(reshape2)

#####----------------#1111111. 결측치 0----------------------------
##train data set
set.seed(0101)
a<-df[df$단계=="경계",]
b<-df[df$단계=="위험",]
c<-df[df$단계=="주의",]
d<-df[df$단계=="초기",]


##train data set
idx1<-sample(1:nrow(df[df$단계=="경계",]),nrow(df[df$단계=="경계",])*0.7)
idx2<-sample(1:nrow(df[df$단계=="위험",]),nrow(df[df$단계=="위험",])*0.7)
idx3<-sample(1:nrow(df[df$단계=="주의",]),nrow(df[df$단계=="주의",])*0.7)
idx4<-sample(1:nrow(df[df$단계=="초기",]),nrow(df[df$단계=="초기",])*0.7)


train1<-a[idx1,] ;test1<-a[-idx1,]
train2<-b[idx2,] ;test2<-b[-idx2,]
train3<-c[idx3,] ;test3<-c[-idx3,]
train4<-d[idx4,] ;test4<-d[-idx4,]

train<-rbind(train1,train2,train3,train4)
test<-rbind(test1,test2,test3,test4)

## 신경망 모형 생성
model <- nnet(단계~.,data=train,size=2,rang=0.1,decay=5e-4,maxit=200)

##신경망 시각화
plot.nnet(model)

# train 예측
pred<-predict(model,newdata=train,type='class')
confusionMatrix(as.factor(pred),train$단계)

# test 예측
pred<-predict(model,newdata=test,type='class')
confusionMatrix(as.factor(pred),test$단계)

#####----------------#1111111. 결측치 -1 ----------------------------
##train data set
set.seed(0101)
a<-df1[df1$단계=="경계",]
b<-df1[df1$단계=="위험",]
c<-df1[df1$단계=="주의",]
d<-df1[df1$단계=="초기",]


##train data set
idx1<-sample(1:nrow(df1[df1$단계=="경계",]),nrow(df1[df1$단계=="경계",])*0.7)
idx2<-sample(1:nrow(df1[df1$단계=="위험",]),nrow(df1[df1$단계=="위험",])*0.7)
idx3<-sample(1:nrow(df1[df1$단계=="주의",]),nrow(df1[df1$단계=="주의",])*0.7)
idx4<-sample(1:nrow(df1[df1$단계=="초기",]),nrow(df1[df1$단계=="초기",])*0.7)


train1<-a[idx1,] ;test1<-a[-idx1,]
train2<-b[idx2,] ;test2<-b[-idx2,]
train3<-c[idx3,] ;test3<-c[-idx3,]
train4<-d[idx4,] ;test4<-d[-idx4,]

train<-rbind(train1,train2,train3,train4)
test<-rbind(test1,test2,test3,test4)
## 신경망 모형 생성
model <- nnet(단계~.,data=train,size=2,rang=0.1,decay=5e-4,maxit=200)

##신경망 시각화
plot.nnet(model)

# train 예측
pred<-predict(model,newdata=df1,type='class')
confusionMatrix(as.factor(pred),df1$단계)

# test 예측
pred<-predict(model,newdata=test,type='class')
confusionMatrix(as.factor(pred),test$단계)

#===========군집화========================================================================
library(NbClust)
library(factoextra)

#군집수
nc<-NbClust(df[,-6],min.nc=3, max.nc=12, method="kmeans")




#kmean 클러스터 적용 k= 4==================
km<- kmeans(df[,-6],4)

#각 변수들과 군집간의 관계
plot(df[,-6],pch=km$cluster,col=km$cluster)

#군집과 단계 비교
table(km$cluster,df$단계)

#군집과 단계 비교
dfk_3<-data.frame(table(km$cluster,df$단계))

x<-dfk_3%>%
  group_by(Var1)%>%
  summarize(sum(Freq))

dfk_3<-merge(dfk_3,x,key=Var1)

dfk_3$비율<-dfk_3$Freq/dfk_3$`sum(Freq)`*100


#
table(km$cluster,df$단계)


#kmean 클러스터 적용 k= 12==================
km<- kmeans(df[,-6],12)

#각 변수들과 군집간의 관계
plot(df[,-6],pch=km$cluster,col=km$cluster)

#군집과 단계 비교
table(km$cluster,df$단계)

#군집과 단계 비교
dfk_3<-data.frame(table(km$cluster,df$단계))

x<-dfk_3%>%
  group_by(Var1)%>%
  summarize(sum(Freq))

dfk_3<-merge(dfk_3,x,key=Var1)

dfk_3$비율<-dfk_3$Freq/dfk_3$`sum(Freq)`*100


#
table(km$cluster,df$단계)


#kmean 클러스터 적용 k= 8==================
km<- kmeans(df[,-6],8)

#각 변수들과 군집간의 관계
plot(df[,-6],pch=km$cluster,col=km$cluster)

#군집과 단계 비교
table(km$cluster,df$단계)

#군집과 단계 비교
dfk_3<-data.frame(table(km$cluster,df$단계))

x<-dfk_3%>%
  group_by(Var1)%>%
  summarize(sum(Freq))

dfk_3<-merge(dfk_3,x,key=Var1)

dfk_3$비율<-dfk_3$Freq/dfk_3$`sum(Freq)`*100


#
table(km$cluster,df$단계)


#kmean 클러스터 적용 k= 6==================
km<- kmeans(df[,-6],6)

#각 변수들과 군집간의 관계
plot(df[,-6],pch=km$cluster,col=km$cluster)

#군집과 단계 비교
table(km$cluster,df$단계)

#군집과 단계 비교
dfk_3<-data.frame(table(km$cluster,df$단계))

x<-dfk_3%>%
  group_by(Var1)%>%
  summarize(sum(Freq))

dfk_3<-merge(dfk_3,x,key=Var1)

dfk_3$비율<-dfk_3$Freq/dfk_3$`sum(Freq)`*100


#
table(km$cluster,df$단계)

#kmean 클러스터 적용 k= 3==================
km<- kmeans(df[,-6],3)

#각 변수들과 군집간의 관계
plot(df[,-6],pch=km$cluster,col=km$cluster)

#군집과 단계 비교
table(km$cluster,df$단계)

#군집과 단계 비교
dfk_3<-data.frame(table(km$cluster,df$단계))

x<-dfk_3%>%
  group_by(Var1)%>%
  summarize(sum(Freq))

dfk_3<-merge(dfk_3,x,key=Var1)

dfk_3$비율<-dfk_3$Freq/dfk_3$`sum(Freq)`*100


#
table(km$cluster,df$단계)

#====다중회귀================================
#안되지 factor 형인데































