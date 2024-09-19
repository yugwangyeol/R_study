if (!require(caret)) {install.packages(('caret'))} ; library(caret)
if (!require(kernlab)) {install.packages(('kernlab'))} ; library(kernlab)
if (!require(tree)) {install.packages(('tree'))} ; library(tree)#버전문제
if (!require(party)) {install.packages(('party'))} ; library(party)
if (!require(e1071)) {install.packages(('e1071'))} ; library(e1071)
#데이터 불러오기
A_train<-read.csv('A_train.csv')
B_test<-read.csv('B_test.csv')
A_train$diagnosis<-as.factor(A_train$diagnosis)

#hold-out방법으로 성능체크
inTrain <- createDataPartition(y = A_train$diagnosis, p = 0.75, list = FALSE)
training <- A_train[inTrain, ]
testing <- A_train[-inTrain, ]
set.seed(32343)
modelFit <- train(diagnosis~ ., data = training, method = "rpart")
predictions <- predict(modelFit, newdata = testing)
confusionMatrix(predictions, testing$diagnosis)

#새로운 데이터의 y값 예측하기
predict <- predict(modelFit, newdata = B_test)

#제출할 파일 제출
sub<-data.frame(diagnosis=predict)
write.csv(sub,'sub.csv',row.names = FALSE)

grid=expand.grid(maxdepth=c(2))
modelFit<-train(diagnosis~.,data=training,method='rpart2',tunegrid=grid,trControl=cv)

modelFit<-train(diagnosis~.,data=A_train,method='rpart2',tunegrid=grid,trControl=cv)
predict<-predict(modelFit,newdata=A_train)
confusionMatrix(predictions,A_train$diagnosis)
