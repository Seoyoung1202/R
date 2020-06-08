music<-read.csv('c:/users/home/desktop/2019_1학기/다변량/팀프로젝트/data.csv')
music$loudness<-music$loudness+60
music$time_signature<-factor(music$time_signature)
music$mode<-factor(music$mode)
music$key<-factor(music$key)
music$target<-factor(music$target)
music<- music[,-c(1,16,17)]
summary(music)
attach(music)

#PCA
music_in<-music[,-c(6,9,12,14)]
result.R<-princomp(music_in, cor=TRUE)
result.R
summary(result.R)
result.R$loadings


##result.R$scores[,1:5] 까지의 변수 사용하시면 돼요!!

data<-result.R$scores[,1:5]
head(data)
dim(data)

pca.LDA<-lda(data,music$target)
pca.LDA

mean(as.numeric(as.character(predict(pca.LDA)$class)==music$target))
mean(as.numeric(as.character(predict(pca.LDA)$class)!=music$target))

pca.LDA<-lda(data,music$mode)
pca.LDA

mean(as.numeric(as.character(predict(pca.LDA)$class)==music$mode))
mean(as.numeric(as.character(predict(pca.LDA)$class)!=music$mode))

data<-data[-1951,]
music<-filter(music, music$time_signature != 1)
pca.LDA<-lda(data,music$time_signature)
pca.LDA
mean(as.numeric(as.character(predict(pca.LDA)$class)==music$time_signature))
mean(as.numeric(as.character(predict(pca.LDA)$class)!=music$time_signature))
music$time_signature[1951]
table(music[-1951,]$time_signature)
