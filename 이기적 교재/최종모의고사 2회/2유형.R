library(dplyr)
library(tidyr)
library(class)
library(caret)

# iris 데이터셋을 이용하여 각 데이터에 따른 종의 분류를 시행해보고 이에 대한 결과에 대해서

# 분석 조건 : 기존의 데이터 무작위 화(randomizing)하여 사용
# 총 150개의 데이터 중 130개를 사용

data(iris)
set.seed(101)
parts <- sample(
    1:nrow(iris),
    size = 130
)

train <- iris[parts, ][-5]
test <- iris[-parts, ][-5]

train_sp <- iris[parts, 5]
test_sp <- iris[-parts, 5]

model <- preProcess(
    train,
    method = c("range")
)

sc_train <- predict(
    model,
    train
)

sc_test <- predict(
    model,
    test
)


k10 <- knn(train = sc_train, test = sc_test, cl = train_sp, k=10)
table(factor(k10))
table(test_sp, k10)

cm10 <- confusionMatrix(
    test_sp,
    k10
)

cm10.acc <- cm10$overall[1]
print(cm10)
print(cm10.acc)

########################################################################### k=60 일 때, ##########################################################################

k60 <- knn(train = sc_train, test = sc_test, cl = train_sp, k=60)
table(factor(k60))
table(test_sp, k60)

cm60 <- confusionMatrix(
    test_sp,
    k60
)

cm60.acc <- cm60$overall[1]
print(cm60)
print(cm60.acc)