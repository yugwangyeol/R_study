library(dplyr)
if(!require(caret)) install.packages("caret"); library(caret)
if(!require(e1071)) install.packages("e1071"); library(e1071)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)


dat = read.csv("dat.csv")
head(dat)
str(dat)
dat$is_cancer = as.factor(dat$is_cancer)
dat <- dat[-1]


intrain<- createDataPartition(y=dat$is_cancer,p=.7,list=F)

training<-dat[intrain,]
testing<-dat[-intrain,]

intrain_1<- createDataPartition(y=training$is_cancer,p=.66,list=F)
OOB_train<-training[intrain_1,]
OOB_test<-training[-intrain_1,]

ntree=15
mtry=3
list_a<-c()
list_b<-c()
for(i in 1:ntree){
  random<-sample(1:30,mtry)
  sample <- sample_frac(OOB_train[,],0.1)
  sample_1 <- sample %>% select(31,random)
  modelfit <- train(is_cancer~.,data=sample_1,method='rpart')
  predictions<-predict(modelfit,newdata=OOB_test)
  b<-confusionMatrix(predictions,OOB_test$is_cancer)
  list_b[i]<-b$byClass[11]
  prediction <-predict(modelfit,newdata=testing)
  a<-confusionMatrix(prediction,testing$is_cancer)
  list_a[i]<-a$byClass[11]
  
}
mean(list_a)
mean(list_b)


modelFit<-train(is_cancer~.,data=training,method='rpart')
pred<-predict(modelfit,newdata=testing)
confusionMatrix(pred,testing$is_cancer)
