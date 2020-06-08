## Time signature

music<-read.csv('c:/users/home/desktop/2019_1학기/다변량/팀프로젝트/data.csv')
music$loudness<-music$loudness+60

library(tidyverse)
library(MASS)
library(ggplot2)
library(biotools)

# time signature 외 볌주형 뺀 데이터 
music<-filter(music, music$time_signature != 1)
data<-music[,-c(1,7,10,15,16,17)]

boxM(data[,-10],data$time_signature) # 등분산성 기각
head(data[,-10])

# LDA
lda.result<-lda(time_signature~.,data=data)
lda.result

mean(as.numeric(as.character(predict(lda.result,data)$class)==data$time_signature))
mean(as.numeric(as.character(predict(lda.result,data)$class)!=data$time_signature))

# QDA
qda.result<-qda(time_signature~.,data=data)
qda.result

mean(as.numeric(as.character(predict(qda.result,data)$class)==data$time_signature))
mean(as.numeric(as.character(predict(qda.result,data)$class)!=data$time_signature))


pred=predict(lda.result,data)
table(true=data$time_signature, predicted=pred$class)

# train/test set
table(data$time_signature)
data[order(data$time_signature),]$time_signature[1985]

music<-music[order(music$time_signature),]
train<-c(sample(1:93,size=74,replace=F),
         sample(94:1984,size=1513,replace=F),
         sample(1985:2016,size=26,replace=F))
train.data<-music[train,-c(1,7,10,13,15,16,17)]
train.target<-music[train,13]
test.data<-music[-train,-c(1,7,10,13,15,16,17)]
test.target<-music[-train,13]

train.LDA<-lda(train.data, train.target)
train.LDA
mean(as.numeric(as.character(predict(train.LDA)$class)==train.target))
mean(as.numeric(as.character(predict(train.LDA)$class)!=train.target))
mean(as.numeric(as.character(predict(train.LDA,test.data)$class)==test.target))
mean(as.numeric(as.character(predict(train.LDA,test.data)$class)!=test.target))

train.QDA<-qda(train.data, train.target)
train.QDA
mean(as.numeric(as.character(predict(train.QDA)$class)==train.target))
mean(as.numeric(as.character(predict(train.QDA)$class)!=train.target))
mean(as.numeric(as.character(predict(train.QDA,test.data)$class)==test.target))
mean(as.numeric(as.character(predict(train.QDA,test.data)$class)!=test.target))


error.list<-which(data$time_signature!=predict(lda.result)$class)
LD<-predict(lda.result)$x
plot(LD, type='p', col=data$time_signature, pch=16)
#points(LD, col=as.numeric(data$time_signature)+2, pch=16, col=c('blue','red','green'))
points(LD[error.list,], col=2, pch=4, cex=2)
