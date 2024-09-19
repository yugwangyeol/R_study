# library
install.packages('e1071')
library(e1071) # svm
library(dplyr) # data wrangling

# load data
# iris 내장 데이터셋 사용
iris %>% head()

# base model
svm_model <- svm(Species ~ ., data=iris) ; svm_model

# parameter tuning
svm_tune <- tune(svm, train.x=subset(iris, select=-Species),
                 train.y=iris$Species,
                 kernel="radial", # kernel 기법 사용
                 ranges=list(cost=10^(-1:2))) # C 값을 튜닝

print(svm_tune)


# after tune model
svm_model_after_tune <- svm(Species ~ ., data=iris, kernel="radial", cost=10)

# predict
pred <- predict(svm_model_after_tune,subset(iris, select=-Species))
table(pred,iris$Species)
