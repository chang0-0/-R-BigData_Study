library(dplyr)
library(caret)
library(ModelMetrics)
library(randomForest)
library(e1071)


setwd("C:/Users/Samsung/Desktop/빅분기실기준비/빅분기 체험")
list.files()

x_test <- read.csv("X_test.csv", encoding = 'UTF-8-BOM', stringsAsFactor = TRUE)
x_train <- read.csv("X_train.csv", encoding = 'UTF-8-BOM', stringsAsFactor = TRUE)
y_train <- read.csv("y_train.csv", encoding = 'UTF-8-BOM', stringsAsFactor = TRUE)

colSums(is.na(x_test))
colSums(is.na(x_train))
colSums(is.na(y_train))
nrow(x_train)
nrow(y_train)

x_train$환불금액[is.na(x_train$환불금액) ] <- mean(x_train$환불금액, na.rm = TRUE)
x_test$환불금액[is.na(x_test$환불금액) ] <- mean(x_test$환불금액, na.rm = TRUE)

str(x_train)
str(x_test)

x_train <- x_train %>% filter(!cust_id %in% c(1521, 2035))
y_train <- y_train %>% filter(!cust_id %in% c(1521, 2035))
y_train$genfac <- ifelse(y_train$gender == 1, "남자", "여자")
summary(x_train)

######################################################################################################

parts <- createDataPartition(
    y_train$gender,
    p = 0.7,
    list = FALSE
)

x_train <- x_train[parts, ]
y_train <- y_train[parts, ]

x_val <- x_train[-parts, ]
y_val <- y_train[-parts, ]

model <- preProcess(
    x_train[, -1],
    method = c("range")
)

x_train_proc <- predict(
    model,
    x_train
)

x_valid_proc <- predict(
    model,
    x_val
)

x_test_proc <- predict(
    model,
    x_test
)

y_train$gender <- as.factor(as.numeric(y_train$gender))

library(e1071)
sv <- svm(
    y_train$genfac ~ . -cust_id ,
    data = x_train_proc,
)

sv
