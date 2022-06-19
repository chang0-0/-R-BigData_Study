library(dplyr)
library(caret)
library(tidyr)
library(ModelMetrics)
library(randomForest)
library(readr)
library(ggplot2)
library(pROC)
library(ROCR)

#  Reached.on.Time_Y.N 컬럼 (1: 정시배송 0 : 정시미배송)

x_train <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/shipping/X_train.csv',
    encoding = 'UTF-8',
    na.strings = c('', ' ', 'NA', NA)
)

y_train <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/shipping/y_train.csv',
    encoding = 'UTF-8',
    na.strings = c('', ' ', 'NA', NA)
)

x_test <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/shipping/X_test.csv',
    encoding = 'UTF-8',
    na.strings = c('', ' ', 'NA', NA)
)

x_label <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/shipping/y_test.csv',
    encoding = 'UTF-8',
    na.strings = c('', ' ', 'NA', NA)
) 

full <- merge(x_train, y_train, by = 'ID')


# 결측값 처리
colSums(is.na(full))
colSums(is.na(x_test))
full$Customer_care_calls <- ifelse(is.na(full$Customer_care_calls), '0', full$Customer_care_calls)
full$Customer_care_calls <- as.factor(full$Customer_care_calls)


x_test$Customer_care_calls <- ifelse(is.na(x_test$Customer_care_calls), '0', x_test$Customer_care_calls)
x_test$Customer_care_calls <- as.factor(x_test$Customer_care_calls)


# 데이터 정제

str(full)
str(x_test)

full$Warehouse_block <- as.factor(full$Warehouse_block)
x_test$Warehouse_block <- as.factor(x_test$Warehouse_block)

full$Mode_of_Shipment <- as.factor(full$Mode_of_Shipment)
x_test$Mode_of_Shipment <- as.factor(x_test$Mode_of_Shipment)

full$Gender <- as.factor(full$Gender)
x_test$Gender <- as.factor(x_test$Gender)

full$Product_importance <- as.factor(full$Product_importance)
x_test$Product_importance <- as.factor(x_test$Product_importance)

full$Customer_rating <- as.factor(full$Customer_rating)
x_test$Customer_rating <- as.factor(x_test$Customer_rating)

full$Reached.on.Time_Y.N <- as.factor(full$Reached.on.Time_Y.N)


full$Prior_purchases <- ifelse(full$Prior_purchases == 10, 8, full$Prior_purchases)
full$Prior_purchases <- ifelse(full$Prior_purchases == 7, 6, full$Prior_purchases)
full$Prior_purchases <- ifelse(full$Prior_purchases == 6, 5, full$Prior_purchases)
full$Prior_purchases <- ifelse(full$Prior_purchases == 8, 6, full$Prior_purchases)


x_test$Prior_purchases <- ifelse(x_test$Prior_purchases == 10, 8, x_test$Prior_purchases)
x_test$Prior_purchases <- ifelse(x_test$Prior_purchases == 7, 6, x_test$Prior_purchases)
x_test$Prior_purchases <- ifelse(x_test$Prior_purchases == 6, 5, x_test$Prior_purchases)
x_test$Prior_purchases <- ifelse(x_test$Prior_purchases == 8, 6, x_test$Prior_purchases)


# 데이터 스케일링

model <- preProcess(
    full[, -c(1)],
    method = c('range')
)

full <- predict(
    model,
    full
)

model <- preProcess(
    x_test[, -c(1)],
    method = c('range')
)

x_test <- predict(
    model,
    x_test
)

rf <- randomForest(
    Reached.on.Time_Y.N ~ . -ID,
    full,
    do.trace = TRUE,
    ntree = 400
)


pred <- predict(
    rf,
    newdata = x_test,
)

list <- x_label$Reached.on.Time_Y.N
list <- as.factor(list)

caret::confusionMatrix(
    list,
    pred,
    mode = 'everything',
    positive = '1'
)

# Accuracy : 0.6628
# F1 : 0.6828

# 랜덤포레스트 AUC 구하기
temp <- prediction(
    x_label$Reached.on.Time_Y.N, 
    pred
)

temp2 <- performance(
    temp,
    measure = 'auc'
)@y.values[[1]]

temp2
# AUC 0.6676582


library(e1071)
sv <- svm(
    Reached.on.Time_Y.N ~ . -ID,
    full,
)

sv.pred <- predict(
    sv,
    newdata = x_test,
    type = 'class'
)

head(sv.pred)

list <- x_label$Reached.on.Time_Y.N
list <- as.factor(list)

caret::confusionMatrix(
    list,
    sv.pred,
    mode = 'everything',
    positive = '1'
)

#  Accuracy : 0.6605
#  F1 : 0.6577


# SVM AUC 구하기
temp <- prediction(
    x_label$Reached.on.Time_Y.N, 
    sv.pred
)

temp2 <- performance(
    temp,
    measure = 'auc'
)@y.values[[1]]

temp2
# AUC : 0.6892109

result <- data.frame(
    x_test$ID,
    sv.pred
)

names(result) <- c('ID', 'Reached.on.Time_Y.N')
head(result)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/데이터마님 빅분기/2유형 연습1/정시 배송 여부 판단")
write.csv(result, "result.csv", row.names = F)

head(x_test$ID)
