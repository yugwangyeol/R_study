data_url = paste0(
  'https://archive.ics.uci.edu/ml/',
  'machine-learning-databases/wine-quality/',
  'winequality-red.csv')

data = read.csv(url(data_url), sep = ';')

head(data)
dim(data)
summary(data)

# 머신러닝을 위한 caret 패키지 설치
if(!require(caret)) install.packages("caret"); library(caret)
if(!require(tidyselect)) install.packages("tidyselect"); library(tidyselect)

##################
#      EDA       #
##################

pairs(data)
boxplot(data)

hist(data$quality)

##################
#      전처리    #
##################   

# 전처리 과정은 생략

##################
# 데이터 셋 분할 #
##################

set.seed(1234) # 난수고정
idx = createDataPartition(data$quality, p=.7, list=F) # 층화추출 , y인 열의 빈도를 기준으로 train 0.7 , test 0.3 의 비율로 구분하겠다.

data_train = data[idx, ]
data_test  = data[-idx, ]

dim(data_train)
dim(data_test)

( dim(data_train)[1] / (dim(data_train)[1] + dim(data_test)[1]) )*100 # 7:3의 비율로 나뉜 것을 알 수 있음.


####################
# 모형선택 및 훈련 #
####################

knn = train(
  quality ~ .,             # 예측하고자 하는 y 값는 quality라는 열 ! 
  data=data_train,
  method='knn',
  tuneGrid=data.frame(.k = 3))

knn

y_train_pred = predict(knn, data_train)
hist(y_train_pred)

# 학습 평가
plot(data_train$quality,y_train_pred)
abline(a=0,b=1,col="black",lty=6)

RMSE(data_train$quality, y_train_pred)

# 테스트 데이터 적용 
y_pred = predict(knn, data_test)
plot(data_test$quality,y_pred)
abline(a=0,b=1,col="black",lty=6)


RMSE(data_test$quality, y_pred)




########################
# 간단한 파라미터 튜닝 #
########################

knn_3 = train(
  quality ~ .,
  data=data_train,
  method='knn',
  preProc = c('center', 'scale'),
  tuneGrid=data.frame(.k = seq(1, 50, 5)))

knn_3



# 평가
y_train_pred = predict(knn_3, data_train)

plot(data_train$quality,y_train_pred)
abline(a=0,b=1,col="black",lty=6)

RMSE(data_train$quality, y_train_pred)


# 테스트 데이터 적용
y_pred = predict(knn_3, data_test)

plot(data_test$quality,y_pred)
abline(a=0,b=1,col="black",lty=6)

RMSE(data_test$quality, y_pred)



#############################
# 교차검증(CrossValidation) #
#############################

require("class")

dim(data)[1]/3 # 533

# get fold no for each rows 
group_1 <- cut(seq(1,533),breaks=5,labels=FALSE)
group_2 <- cut(seq(534,1066),breaks=5,labels=FALSE)
group_3 <- cut(seq(1067,1599),breaks=5,labels=FALSE)
fold <- c(group_1, group_2, group_3)
acc <- c() # accuracy for each fold

for (i in 1:5){
  ds.tr <- data[fold != i, 1:11]
  ds.ts <- data[fold == i, 1:11]
  cl.tr <- data[fold != i, 12]
  cl.ts <- data[fold == i, 12]
  pred <- knn(ds.tr, ds.ts, cl.tr, k = 3)
  acc[i] <- mean(pred==cl.ts) # 예측 정확도
}

?knn

acc # accuracy of 5 fold

mean(acc) # mean accuracy of 5 fold
