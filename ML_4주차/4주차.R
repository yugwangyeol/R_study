### 메모리 비우기 
rm(list=ls())
gc()

### R 3.6.0 version

### setwd 
setwd("workingdirectory")

### packages
if(!require(caret)) install.packages("caret"); library(caret)
if(!require(e1071)) install.packages("e1071"); library(e1071)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)
library(lubridate)


### data 불러오기
dat = read.csv("dat.csv")
head(dat)
str(dat)
dat$is_cancer = as.factor(dat$is_cancer)
dat <- dat[-1]

### train / test 분할
set.seed(1)
prop.table(table(dat$is_cancer))

idx = createDataPartition(dat$is_cancer, p = 0.7, list=F)
train = dat[idx,]
test = dat[-idx,]
prop.table(table(train$is_cancer))
prop.table(table(test$is_cancer))

str(train)

### randomforest
control = trainControl(method='repeatedcv', search='random', number=5,repeats = 10,verbose = TRUE)
rf.model <- train(
  is_cancer ~ .,
  data = train,
  tuneLength = 3,
  trControl = control,
  method="rf")
rf.model

pred.rf <- predict(rf.model,test[-31])
confusionMatrix(pred.rf, test[,31])

rf.grid = expand.grid(
  mtry = c(1,3,5)          # 노드를 나눌 기준을 정할 때 고려할 변수의 수
)

control = trainControl(method='repeatedcv', search='grid', number=5,repeats = 3,verbose = TRUE)
rf.model <- train(
  is_cancer ~ .,
  data = test,
  tuneGrid = rf.grid,
  metric = 'Accuracy',
  trControl = control,
  method = 'rf'
)
rf.model

pred.rf <- predict(rf.model,test[-31])
confusionMatrix(pred.rf, test[,31])

##변수 중요도
m <- randomForest(is_cancer ~., data=train, importance=TRUE)
importance(m)
varImpPlot(m, main="varImpPlot of cancer")

##confusion matrix
a <- confusionMatrix(pred.rf, test[,31])
a$table
a$byClass


### 연습장

a<-hour(ymd_h(train_c$TIME_ID))

a<- data.frame(a)
class(a[1])
HF_tr<- data.frame()

for (i in 1:2500){
  c<-train_c %>% filter(CUS_ID==1) %>% filter( a==0 | a==1 | a==2 | a==3 | a==4 | a==5 ) %>% summarise(HF_tr=sum(ST_TIME))
  
}
