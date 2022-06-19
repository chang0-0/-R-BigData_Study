library(dplyr)
library(caret)
library(randomForest)
library(tidyr)
library(readr)
library(scales)
library(ModelMetrics)

# 자동차 보험 가입 예측 Response 컬럼 예측

train <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/insurance/train.csv',
    encoding = 'UTF-8',
    na.strings = c("", " ", "NA", NA)
)

test <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/insurance/test.csv',
    encoding = 'UTF-8',
    na.strings = c("", " ", "NA", NA)
)

sub <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/insurance/submission.csv',
    encoding = 'UTF-8',
    na.strings = c("", " ", "NA", NA)
)


# 결측값은 하나도 없음
colSums(is.na(train))
colSums(is.na(test))

# id컬럼은 제거
train <- train[, -1]
str(train)

train$Gender <- as.factor(train$Gender)
test$Gender <- as.factor(test$Gender)

train$Driving_License <- ifelse(train$Driving_License == '1', 'Yes', 'No')
train$Driving_License <- as.factor(train$Driving_License)

test$Driving_License <- ifelse(test$Driving_License == '1', 'Yes', 'No')
test$Driving_License <- as.factor(test$Driving_License)

train$Response <- ifelse(train$Response == 0, 'No', 'Yes')
train$Response <- as.factor(train$Response)

train$Previously_Insured <- ifelse(train$Previously_Insured == 1, 'Yes', 'No')
train$Previously_Insured <- as.factor(train$Previously_Insured)

test$Previously_Insured <- ifelse(test$Previously_Insured == 1, 'Yes', 'No')
test$Previously_Insured <- as.factor(test$Previously_Insured)

train$Vehicle_Age <- as.factor(train$Vehicle_Age)
test$Vehicle_Age <- as.factor(test$Vehicle_Age)


model <- preProcess(
    train,
    mehtod = c('range')
)

train <- predict(
    model,
    train
)


model <- preProcess(
    test[-1],
    mehtod = c('range')
)

test <- predict(
    model,
    test
)


# 랜덤포레스트 학습 속도가 너무 느려서 데이터 반으로 절단..
n <- nrow(train) * 0.5
train <- train[1:n, ]


rf <- randomForest(
    Response ~ .,
    train,
    do.trace = TRUE,
    ntree = 400
)

print(auc(rf))

pred <- predict(
    object = rf,
    newdata = test
)

pred <- ifelse(pred == 'No' , 0, 1)
pred <- as.factor(pred)

result <- data.frame(
    test$id,
    pred
)

names(result) <- c('id', 'Response')
head(result)

setwd('C:/Users/Samsung/Desktop/빅분기실기준비/이기적 스터디카페 실기 자료실/2주차')
write.csv(result, 'result.csv', row.names = F)