library(dplyr)
library(e1071)
library(caret)
library(randomForest)
library(scales)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 제2유형")
list.files()

ds <- read.csv(
    "WA_Fn-UseC_-Telco-Customer-Churn.csv",
    encoding = "UTF-8",
    stringsAsFactor = TRUE
)

nrow(ds)

# 주어진 훈련 데이터를 이용하여 모델을 훈련한 후,
# 테스트 데이터로 고갤의 이탈 여부를 예측하고 CSV 포맷으로 제출하시오.

ds <- na.omit(ds)
head(ds)
str(ds)

# 7:3으로 분리

idx <- createDataPartition(
    ds$Churn, p = 0.8
)

x_train <- ds[idx$Resample1, -21]
x_test <- ds[-idx$Resample1, -21]

y_train <- ds[idx$Resample1, c(1,21)]
y_test <- ds[-idx$Resample1, c(1,21)]

nrow(ds)
nrow(x_train)


idx <- createDataPartition(y_train$Churn, p = 0.8)
x_t <- x_train[idx$Resample1, ]
x_v <- x_train[-idx$Resample1, ]
y_t <- y_train[idx$Resample1, ]
y_v <- y_train[-idx$Resample1, ]

model_prePro <- preProcess(x_t, method = c("range"))

scaled_x_t <- predict(model_prePro, x_t)
scaled_x_v <- predict(model_prePro, x_v)
scaled_x_test <- predict(model_prePro, x_test)

nrow(y_t)
nrow(scaled_x_t)

set.seed(123)
model_rf <- randomForest(
    y_t$Churn ~ . -customerID,
    scaled_x_t,
    ntree = 300,
    do.trace = TRUE
)


predict_rf <- predict(
    model_rf,
    newdata = scaled_x_v, 
    type = "response",
)

caret::confusionMatrix(
    predict_rf, 
    y_v$Churn
)
