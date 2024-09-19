library(dplyr)



dat = read.csv("dat.csv")
head(dat)
str(dat)
dat$is_cancer = as.factor(dat$is_cancer)
dat <- dat[-1]


intrain<- createDataPartition(y=dat$is_cancer,p=.7,list=F)
set.seed(2134)
training<-dat[intrain,]
testing<-dat[-intrain,]

intrain_1<- createDataPartition(y=dat$is_cancer,p=.66,list=F)
OOB_train<-training[intrain_1,]
OOB_test<-training[-intrain_1,]

ntree=10
mtry=3
list_a<-c()
list_b<-c()
for(i in 1:ntree){
  random<-sample(1:30,mtry)
  sample <- sample_frac(OOB_train[,],0.1)
  sample <- na.omit(sample)
  sample_1 <- sample %>% select(31,random)
  modelfit <- train(is_cancer~.,data=sample_1,method='rpart')
  predictions<-predict(modelfit,newdata=OOB_test, type = 'prob')
  b<-confusionMatrix(as.factor(round(predictions[,2])),OOB_test$is_cancer)
  list_b[i]<-b$table
  prediction <-predict(modelfit,newdata=testing)
  a<-confusionMatrix(prediction,testing$is_cancer)
  list_a[i]<-a$byClass[11]
  
}
mean(list_a)
mean(list_b)
