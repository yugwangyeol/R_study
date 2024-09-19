rm(list=ls())

# setting working directory
## Ctrl + Shift + H
setwd()

# packages
if(!require(caret)) install.packages("caret"); library(caret)
if(!require(xgboost)) install.packages("xgboost"); library(xgboost)
if(!require(e1071)) install.packages("e1071"); library(e1071)
if(!require(tictoc)) install.packages("tictoc"); library('tictoc')

# data 불러오기
dat = read.csv("5th_dat.csv")
head(dat)
str(dat)
dim(dat)
dat$is_cancer = as.factor(dat$is_cancer)
dat <- dat[-1] #id열 삭제.

### train / test 나누기(split)
set.seed(1)
prop.table(table(dat$is_cancer))

idx = createDataPartition(dat$is_cancer, p = 0.7, list=F)
train = dat[idx,]
test = dat[-idx,]
prop.table(table(train$is_cancer))
prop.table(table(test$is_cancer))

str(train)
str(test)

### Gradient Boosting
control = trainControl(method='repeatedcv', search='grid', number=5,repeats = 3,verbose = TRUE) # repeatcv : k - fold를 repeats만큼 반복
tic('gbm running time :')
gbm.model <- train(
  is_cancer ~ .,
  data = train, 
  tuneLength = 3,
  trControl = control,
  method="gbm")
toc()

gbm.model$bestTune

pred.gbm <- predict(gbm.model,test[-31])
confusionMatrix(pred.gbm, test[,31])

## GBM Parameter Tuning
gbm.grid = expand.grid(
  shrinkage = c(0.1,0.3, 0.5),  # 학습률
  interaction.depth = c(3,6,9),  # 트리의 최대 깊이
  n.minobsinnode = c(5,10,15),   # 나무 생성을 위한 최소 data 개수
  n.trees = c(500,100,1500)  # 최대 나무 개수
)

control = trainControl(method='repeatedcv', search='grid', number=5,repeats = 2,verbose = TRUE) #number은 fold개수, repeats는 반복횟수.
tic('gbm running time :')
gbm.model <- train(
  is_cancer ~ .,
  data = train,
  tuneGrid = gbm.grid,
  trControl = control,
  method = 'gbm'
)
toc()
pred.gbm <- predict(gbm.model,test[-31])
confusionMatrix(pred.gbm, test[,31])

### eXtreme Gradient Boosting
## parameter tuning
control = trainControl(method='repeatedcv', search='grid', number=5,repeats = 3,verbose = TRUE)
tic('xgb running time :')
xgb.model <- train(
  is_cancer ~ .,
  data = train,
  tuneLength = 4, #실험해볼 parameter 개수.
  trControl = control,
  method="xgbTree")
toc()

xgb.model

pred.xgb <- predict(xgb.model,test[-31])
confusionMatrix(pred.xgb, test[,31])

## XGB Parameter tuning
xgb.grid = expand.grid(
  nrounds = 300,
  eta = 0.05,
  gamma = 5,
  max_depth = 4,
  min_child_weight = 6,
  colsample_bytree = 1,
  subsample = 1
)

control = trainControl(method='repeatedcv', search='grid', number=5,repeats = 3,verbose = TRUE)
xgb.model <- train(
  is_cancer ~ .,
  data = train,
  tuneGrid = xgb.grid, ##설정한 parameter 조합.
  trControl = control,
  method = 'xgbTree'
)
xgb.model

pred.xgb <- predict(xgb.model,test[-31])
confusionMatrix(pred.xgb, test[,31])

# 모델 간 성능 및 시간차이 비교.
control = trainControl(method='repeatedcv', search='grid', number=3,repeats = 2,verbose = FALSE)
## 시간
tic('RF running time :')
rf.model <- train(
  is_cancer ~ .,
  data = train,
  trControl = control,
  method="rf")
toc()

tic('GBM running time :')
gbm.model <- train(
  is_cancer ~ .,
  data = train,
  trControl = control,
  method="gbm")
toc()

tic('XGB running time :')
xgb.model <- train(
  is_cancer ~ .,
  data = train,
  trControl = control,
  method="xgbTree")
toc()

##성능
###RandomForest
pred.rf <- predict(rf.model,test[-31])
rf.result <- confusionMatrix(pred.rf, test[,31])$overall
rf.result[1]
###Gradient Boosting
pred.gbm <- predict(gbm.model,test[-31])
gbm.result <- confusionMatrix(pred.gbm, test[,31])$overall
gbm.result[1]
###eXtreme Gradient Boosting
pred.xgb <- predict(xgb.model,test[-31])
xgb.result <- confusionMatrix(pred.xgb, test[,31])$overall
xgb.result[1]