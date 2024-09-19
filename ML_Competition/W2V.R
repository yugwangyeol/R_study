rm(list = ls())
gc()

setwd('C:\\Users\\USER\\Desktop\\word2vec')

# Install & load word2vec package.
library(devtools)
library(wordVectors)
library(dplyr)
# Install & load data.table package
library(data.table)
library(randomForest)
library(caret)
devtools::install_github("bmschmidt/wordVectors")
# list objects in word2vec package
ls("package:wordVectors")

###### Fast reading and combining several files using data.table (with fread): 5 times faster than read.csv()
te <- fread("test_clickstreams.tab")
cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_clickstreams.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID) 
md.dt <- merge(cs.dt, tr.dt)
md.dt %>% str

dis_te<-dis_te[,-43]
te.dt<-dis_te[,c(1,43)]
te[,CUS_ID:=as.numeric(CUS_ID)]
setkey(w2v_te, CUS_ID); setkey(te, CUS_ID) 
md.te <- merge(w2v_te, te)
te.dt<-data.frame(te.dt)
str(te.dt)
str(cs.dt)

w2v_te<-as.data.table(w2v_te)
te<-as.data.table(te)
w2v_te$gf<-as.character(w2v_te$gf) 
str(w2v_te)

te[,CUS_ID:=as.numeric(CUS_ID)]
names(w2v_te)[1]<-c('CUS_ID')
w2v_te<-w2v_te[,-3]

###### 단어장 만들기
f_te <- function(x, t) {
  grp <- md.te[CUS_ID==x, GROUP][1]
  itemfreq <- table(md.te[CUS_ID==x,  ACT_NM])
  fitems <- itemfreq[itemfreq >= t]
  act <- names(fitems)
  #  
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  #
  as.vector((sapply(1:20, function(x) c(grp, sample(act)))))
}
items <- unlist(sapply(w2v_te$CUS_ID, f_te, 2)) 
write.table(items, "items_t.txt", eol = " ", quote = F, row.names = F, col.names = F) # 단어장 저장(혹시라도 열어보고 싶다면 주의)

f(te.dt$CUS_ID,2)
md.te$GROUP

f_tr <- function(x, t) {
  grp <- md.dt[CUS_ID==x, GROUP][1]
  itemfreq <- table(md.dt[CUS_ID==x,  ACT_NM])
  fitems <- itemfreq[itemfreq >= t]
  act <- names(fitems)
  #  
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  #
  as.vector((sapply(1:20, function(x) c(grp, sample(act)))))
}
items <- unlist(sapply(cs.dt$CUS_ID, f_tr, 2)) 
write.table(items, "items.txt", eol = " ", quote = F, row.names = F, col.names = F) # 단어장 저장(혹시라도 열어보고 싶다면 주의)






g <- function(x, dt, min) {
  itemfreq <- table(dt[CUS_ID==x, ACT_NM])
  fitems <- itemfreq[itemfreq >= min]
  sim <- cosineSimilarity(model[[names(fitems), average=T]],
                          model[[c("M20-","M30","M40+","F20-","F30","F40+"), average=F]])
  return(names(which.max(sim[1,])))
}

gf<-factor(c(),levels = c('F20-','F30','F40+','M20-','M30','M40+'))

for(i in 2501:5000){
  a<-g(i,te,2)
  gf[i-2500]<-a
}
gf_d<-data.frame(gf)
w2v_te<-data.frame(c(2501:5000),gf_d)


dis_te<-cbind(dis_te,gf_d)
names(dis_te)[43]<-c('GROUP')
dis_tr
dis_te<-dis_te[,-1]
f <- function(x, t) {
  grp <- test_c[CUS_ID==x][1]
  itemfreq <- table(test_c[CUS_ID==x,  ACT_NM])
  fitems <- itemfreq[itemfreq >= t]
  act <- names(fitems)
  #  
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  #
  as.vector((sapply(1:20, function(x) c(grp, sample(act)))))
}
items <- unlist(sapply(test_c$CUS_ID, f,2)) 
write.table(items, "items_t.txt", eol = " ", quote = F, row.names = F, col.names = F) # 단어장 저장(혹시라도 열어보고 싶다면 주의)

##### Train model
set.seed(12345)
model = train_word2vec("items_t.txt","vec_t.bin",
                       vectors=300, # 임베딩차원
                       threads=2, # 사용할 CPU코어 개수 
                       window=7, # window size
                       cbow=1, # 0 == CBOW, 1 == Skip gram 
                       iter=5, # 학습 반복횟수
                       negative_samples=10, # negative sample 비 1 : negative_samples
                       force = T # 모델 덮어쓸지 말지
                       )

## [reload the model]
model <- read.binary.vectors("vec.bin") # 학습된 모델 불러오기 이미 학습을 했다면 위의 코드는 실행하지 않아도 된다.
train <- md.te %>% select(CUS_ID,ACT_NM,GROUP)

##### Explore the model
for (word in unique(md.dt[,GROUP])) print(closest_to(model, word, n=10))

model[[unique(md.dt[,GROUP]), average=F]] %>% plot(method="pca") # group을 2차원 공간에 뿌림

items.1 <- c(unique(md.dt[,GROUP]), unique(md.dt[CUS_ID==1, ACT_NM])) 

model[[items.1[1:100], average=F]] %>% plot(method="pca") #group + 텍스트를 2차원 공간에 뿌림

# 코사인 유사도 계산 
cosineSimilarity(model[[unique(md.dt[CUS_ID==1, ACT_NM]), average=T]],
                 model[[c("M20-","M30","M40+","F20-","F30","F40+"), average=F]])
cosineSimilarity(model[[unique(md.dt[CUS_ID==2, ACT_NM]), average=T]], 
                 model[[c("M20-","M30","M40+","F20-","F30","F40+"), average=F]])

##### Predict & Evaluate
# calculate the cosine similarity between items and target classes
g <- function(x, dt, min) {
  itemfreq <- table(dt[CUS_ID==x, ACT_NM])
  fitems <- itemfreq[itemfreq >= min]
  sim <- cosineSimilarity(model[[names(fitems), average=T]],
                          model[[c("M20-","M30","M40+","F20-","F30","F40+"), average=F]])
  return(names(which.max(sim[1,])))
}

# accuracy for train data
# 조금 오래걸림(버틸만함)
ctab <- table(sapply(cs.dt$CUS_ID, g, md.dt, 1), cs.dt$GROUP); ctab

sum(diag(ctab)) / nrow(cs.dt) 

nrow(cs.dt[GROUP=="M20-",]) / nrow(cs.dt)
nrow(cs.dt[GROUP=="M30",]) / nrow(cs.dt)
nrow(cs.dt[GROUP=="M40+",]) / nrow(cs.dt)
nrow(cs.dt[GROUP=="F20-",]) / nrow(cs.dt)
nrow(cs.dt[GROUP=="F30",]) / nrow(cs.dt)
nrow(cs.dt[GROUP=="F40+",]) / nrow(cs.dt)
