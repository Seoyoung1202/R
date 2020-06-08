## Mode

music<-read.csv('c:/users/home/desktop/2019_1학기/다변량/팀프로젝트/data.csv')
music$loudness<-music$loudness+60

library(tidyverse)
library(MASS)
library(ggplot2)
library(biotools)

# mode 외 볌주형 뺀 데이터 
data<-music[,-c(1,7,13,15,16,17)]
head(data)

boxM(data[,-8],data$mode) # 등분산성 기각
head(data[,-8])

# LDA
lda.result<-lda(mode~.,data=data)
lda.result

mean(as.numeric(as.character(predict(lda.result,data)$class)==data$mode))
mean(as.numeric(as.character(predict(lda.result,data)$class)!=data$mode))

# QDA
qda.result<-qda(mode~.,data=data)
qda.result

mean(as.numeric(as.character(predict(qda.result,data)$class)==data$mode))
mean(as.numeric(as.character(predict(qda.result,data)$class)!=data$mode))

pred=predict(qda.result,data)
table(true=data$mode, predicted=pred$class)

# train/test set
table(data$mode)
data[order(data$mode),]$mode[783]

train<-c(sample(1:782,size=626,replace=F),
         sample(783:2017,size=988,replace=F))
train.data<-music[train,-c(1,7,10,13,15,16,17)]
train.target<-music[train,10]
test.data<-music[-train,-c(1,7,10,13,15,16,17)]
test.target<-music[-train,10]

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
