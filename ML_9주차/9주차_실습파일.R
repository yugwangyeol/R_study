#Loading the required libraries
library('caret')
#Seeting the random seed
set.seed(1)

#Loading the hackathon dataset
data<-read.csv(url('https://datahack-prod.s3.ap-south-1.amazonaws.com/train_file/train_u6lujuX_CVtuZ9i.csv'))

#Let's see if the structure of dataset data
str(data)
#Does the data contain missing values
sum(is.na(data))

#Imputing missing values using median
preProcValues <- preProcess(data, method = c("medianImpute"))
data_processed <- predict(preProcValues, data)

sum(is.na(data_processed))

#Spliting training set into two parts based on outcome: 67% and 33%
index <- createDataPartition(data_processed$Loan_Status, p=0.67, list=FALSE)
trainSet <- data_processed[ index,]
testSet <- data_processed[-index,]

#Defining the training controls for multiple models
fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final',
  classProbs = T)

#Defining the predictors and outcome
predictors<-c("Credit_History", "LoanAmount", "Loan_Amount_Term", "ApplicantIncome",
              "CoapplicantIncome") # X
outcomeName<-'Loan_Status' # y

#Training model
model_knn<-train(trainSet[,predictors],trainSet[,outcomeName],method='knn',trControl=fitControl,tuneLength=3)
model_lr<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm',trControl=fitControl,tuneLength=3)
model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf',trControl=fitControl,tuneLength=3)

#Predicting using model
testSet$pred_knn<-predict(object = model_knn,testSet[,predictors])
testSet$pred_lr<-predict(object = model_lr,testSet[,predictors])
testSet$pred_rf<-predict(object = model_rf,testSet[,predictors])

knn.result <- confusionMatrix(testSet$pred_knn, testSet[,outcomeName])$overall ; knn.result[1]
lr.result <- confusionMatrix(testSet$pred_lr, testSet[,outcomeName])$overall ; lr.result[1]
rf.result <- confusionMatrix(testSet$pred_rf, testSet[,outcomeName])$overall ; rf.result[1]

#The majority vote
testSet$pred_majority<-as.factor(ifelse(testSet$pred_knn=='Y' & testSet$pred_lr=='Y','Y',ifelse(testSet$pred_knn=='Y' & testSet$pred_rf=='Y','Y',ifelse(testSet$pred_lr=='Y' & testSet$pred_rf=='Y','Y','N'))))
voting.result <- confusionMatrix(testSet$pred_majority, testSet[,outcomeName])$overall ; voting.result[1]

#Predicting the probabilities for averaging
testSet$pred_knn_prob<-predict(object = model_knn,testSet[,predictors],type='prob')
testSet$pred_lr_prob<-predict(object = model_lr,testSet[,predictors],type='prob')
testSet$pred_rf_prob<-predict(object = model_rf,testSet[,predictors],type='prob')

#Taking average of predictions
testSet$pred_avg<-(testSet$pred_knn_prob$Y+testSet$pred_lr_prob$Y+testSet$pred_rf_prob$Y)/3

#Splitting into binary classes at 0.5
testSet$pred_avg<-as.factor(ifelse(testSet$pred_avg>0.5,'Y','N'))

averaging.result <- confusionMatrix(testSet$pred_avg, testSet[,outcomeName])$overall ; averaging.result[1]

#Defining the training control
fitControl <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = 'final', # To save out of fold predictions for best parameter combinantions
  classProbs = T # To save the class probabilities of the out of fold predictions
)

#Defining the predictors and outcome
predictors<-c("Credit_History", "LoanAmount", "Loan_Amount_Term", "ApplicantIncome",
              "CoapplicantIncome")
outcomeName<-'Loan_Status'

#Training the random forest model
model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf',trControl=fitControl,tuneLength=3)

#Training the knn model
model_knn<-train(trainSet[,predictors],trainSet[,outcomeName],method='knn',trControl=fitControl,tuneLength=3)

#Training the logistic regression model
model_lr<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm',trControl=fitControl,tuneLength=3)

#Predicting the out of fold prediction probabilities for training data
trainSet$OOF_pred_rf<-model_rf$pred$Y[order(model_rf$pred$rowIndex)]
trainSet$OOF_pred_knn<-model_knn$pred$Y[order(model_knn$pred$rowIndex)]
trainSet$OOF_pred_lr<-model_lr$pred$Y[order(model_lr$pred$rowIndex)]

#Predicting probabilities for the test data
testSet$OOF_pred_rf<-predict(model_rf,testSet[predictors],type='prob')$Y
testSet$OOF_pred_knn<-predict(model_knn,testSet[predictors],type='prob')$Y
testSet$OOF_pred_lr<-predict(model_lr,testSet[predictors],type='prob')$Y                

#Predictors for top layer models 
predictors_top<-c('OOF_pred_rf','OOF_pred_knn','OOF_pred_lr') 

#GBM as top layer model 
model_gbm<- train(trainSet[,predictors_top],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneLength=3)
#Logistic regression as top layer model
model_glm<- train(trainSet[,predictors_top],trainSet[,outcomeName],method='glm',trControl=fitControl,tuneLength=3)

#predict using GBM top layer model
testSet$gbm_stacked<-predict(model_gbm,testSet[,predictors_top])
stacking_gbm.result <- confusionMatrix(testSet$gbm_stacked, testSet[,outcomeName])$overall ; stacking_gbm.result[1]

#predict using logictic regression top layer model
testSet$glm_stacked<-predict(model_glm,testSet[,predictors_top])
stacking_glm.result <- confusionMatrix(testSet$glm_stacked, testSet[,outcomeName])$overall ; stacking_glm.result[1]
