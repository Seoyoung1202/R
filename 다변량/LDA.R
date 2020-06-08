music<-read.csv('c:/users/home/desktop/2019_1학기/다변량/팀프로젝트/data.csv')
music$loudness<-music$loudness+60

library(tidyverse)
library(MASS)
library(ggplot2)

dim(music)
head(music)

# numerical variables
data<-music[,-c(1,7,10,13,15,16,17)]
target<-music$target
mode<-music$mode
time_signature<-music$time_signature

pairs(data,col=as.numeric(target)+2, pch=16)
target.LDA<-lda(data,target)
target.LDA

target[1021] # 부터 0
# 8:2 train:test
train<-c(sample(1:1020,size=816,replace=F),
         sample(1021:2017,size=798,replace=F))
train.data<-music[train,-c(1,7,10,13,15,16,17)]
train.target<-music[train,15]
test.data<-music[-train,-c(1,7,10,13,15,16,17)]
test.target<-music[-train,15]
#head(train.target)
train.LDA<-lda(train.data, train.target)
train.LDA

mean(as.numeric(as.character(predict(train.LDA)$class)==train.target))
mean(as.numeric(as.character(predict(train.LDA)$class)!=train.target))

mean(as.numeric(as.character(predict(train.LDA,test.data)$class)==test.target))
mean(as.numeric(as.character(predict(train.LDA,test.data)$class)!=test.target))
#qda
train.QDA<-qda(train.data, train.target)
train.QDA
mean(as.numeric(as.character(predict(train.QDA)$class)==train.target))
mean(as.numeric(as.character(predict(train.QDA)$class)!=train.target))
mean(as.numeric(as.character(predict(train.QDA,test.data)$class)==test.target))
mean(as.numeric(as.character(predict(train.QDA,test.data)$class)!=test.target))



pairs(music[,-c(1,7,10,13,15,16,17)])

## 판별분석 (target)

# 범주형 포함
music$time_signature<-factor(music$time_signature)
music$mode<-factor(music$mode)
music$key<-factor(music$key)
music$target<-factor(music$target)
data<- music[,-c(1,16,17)]

# target 외 범주형 뺀 데이터
data<-music[,-c(1,7,10,13,16,17)]
ldahist(data$acousticness, g=data$target, type='both')

# factor 안해도 같은 결과가 나옴
# data$target<-factor(data$target)
# str(data$target)

# LDA
lda.result<-lda(target~.,data=data)
summary(lda.result)
lda.result

# predicted value
pred<-predict(lda.result, data)
summary(pred)

pc<-predict(lda.result, data)$class
pc<-as.numeric(pc)
table(pc)

pc[pc==1] <- 0 # pc==1 이면 target=0
pc[pc==2] <- 1 # pc==2 이면 target=1

correct.rate<-mean(data$target==pc); correct.rate
error.rate<-mean(data$target!=pc); error.rate 

pred=predict(qda.result,data)
table(true=data$target, predicted=pred$class)

error.list<-which(data$target!=predict(lda.result)$class)
LD<-predict(lda.result)$x
summary(LD)
plot(LD, type='n')
points(LD, col=as.numeric(data$target), pch=16)
points(LD[error.list,], pch=14, cex=1.5)

# 등분산성 검정 - BoxM test
install.packages('biotools')
library(biotools) 
boxM(data[,-11],data$target)
head(data[,-11])
head(data)

plot(lda.result)
lda.result

# QDA
qda.result<-qda(target~.,data=data)
summary(qda.result)
qda.result

pc<-predict(qda.result, data)$class
pc<-as.numeric(pc)
pc[pc==1] <- 0 # pc==1 이면 target=0
pc[pc==2] <- 1 # pc==2 이면 target=1

correct.rate<-mean(data$target==pc); correct.rate
error.rate<-mean(data$target!=pc); error.rate 






# 등분산성 기각
boxM(data[,-10],data$time_signature)
head(data[,-10])
head(data)

# LDA
lda.result<-lda(time_signature~.,data=data)
lda.result

# predicted value
pred<-predict(lda.result, data)
summary(pred)

pc<-predict(lda.result, data)$class
pc<-as.numeric(pc)
table(pc)

pc[pc==1] <- 3 # pc==1 이면 time_signature=3
pc[pc==2] <- 4 # pc==2 이면 time_signature=4
pc[pc==3] <- 5 # pc==2 이면 time_signature=5


QDA.result<-qda(time_signature~.,data=data)
QDA.result
mean(as.numeric(as.character(predict(QDA.result,data)$class)==data$time_signature))
mean(as.numeric(as.character(predict(QDA.result,data)$class)!=data$time_signature))

correct.rate<-mean(data$time_signature==pc); correct.rate
error.rate<-mean(data$time_signature!=pc); error.rate

install.packages('klaR')
library(klaR)
par(mfrow=c(1,1))
par(mar=c(1,1,1,1))
partimat(data$target~.,data=data, method='lda')
dev.off()

error.list<-which(data$time_signature!=predict(lda.result)$class)
LD<-predict(lda.result)$x
summary(LD)
plot(LD, type='p')
points(LD, col=as.numeric(data$time_signature)+2, pch=16)
points(LD[error.list,], col=2, pch=14, cex=1.5)
summary(LD)

#plot(lda.result$x[,1], lda.result$x[,2])
#text(lda.result$x[,1], lda.result$x[,2], Type, cex = 0.7, pos = 4, col = "red")

install.packages('scales')
library(scales)
prop.lda = lda.result$svd^2/sum(lda.result$svd^2)
dataset = data.frame(target = data[,"target"], lda = predict(lda.result)$x)

p1 <- ggplot(dataset) + geom_point(aes(lda.LD1, lda.LD2, colour = species, shape = species), size = 2.5) + 
  labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""))
