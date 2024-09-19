if (!require(caret)) {install.packages(('caret'))} ; library(caret)
if (!require(kernlab)) {install.packages(('kernlab'))} ; library(kernlab)
if (!require(tree)) {install.packages(('tree'))} ; library(tree)#버전문제
if (!require(party)) {install.packages(('party'))} ; library(party)
if (!require(e1071)) {install.packages(('e1071'))} ; library(e1071)
data(spam)
str(spam)
##################### caret패키지를 이용한 rpart ####################
inTrain <- createDataPartition(y = A_train$diagnosis, p = 0.7, list = FALSE)
training <- A_train[inTrain, ]
testing <- A_train[-inTrain, ]
set.seed(32343)
modelFit <- train(diagnosis ~ ., data = training, method = "rpart")
predictions <- predict(modelFit, newdata = testing)
confusionMatrix(predictions, testing$diagnosis)

#### 사후 가지치기(자동)##############우리가 파라미터 범위를 지정하지 않아도 프로그램상에서 random적으로 알아서 정해준다.

trctrl <- trainControl(method = "cv", number = 5, verbose=T,search='random')
#trainControl(method = "repeatedcv", number = 5, repeats=5,verbose=T)#5-fold crossvalidation을 또 5번 반복(25번 실행)

modelFit <- train(diagnosis ~ ., data = training, method = "rpart2",trControl=trctrl,tuneLength=20)
#tuneLength: random적으로 파라미터를 몇번 찾을건지.

modelFit

predictions <- predict(modelFit, newdata = testing)

confusionMatrix(predictions, testing$diagnosis)

#### 사후 가지치기 (그리드)############


cv <- trainControl(method = "cv", number = 5, verbose = T)
#Cross_validation  #verbose를 해놔야 과정내역확인가능

grid = expand.grid(maxdepth = c(2,3,4,5,6,7))

modelFit <- train(diagnosis ~ ., data = training, method = "rpart2",tuneGrid = grid,trControl=cv)


modelFit
#rpart2로 한 이유는 rpart에서는 파라미터 cp밖에 지원을 안해주고 2는 maxdepth까지 지원을 해준다고 함.

predictions <- predict(modelFit, newdata = testing)

confusionMatrix(predictions, testing$diagnosis)

modelFit<-train(diagnosis~.,data=A_train,method='rpart2',tunegrid=grid,trControl=cv)
predict<-predict(modelFit,newdata=A_train)
confusionMatrix(predict,A_train$diagnosis)
#######################party 패키지 ###########################
partymod<-ctree(diagnosis~., data=training)
predictions <- predict(partymod, newdata = testing)
confusionMatrix(predictions, testing$diagnosis)
