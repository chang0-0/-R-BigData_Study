library(dplyr)
library(caret)
library(randomForest)
library(e1071)
library(ROCR)

# https://datawithnosense.tistory.com/40

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/기출2회")
list.files()
main <- read.csv(
    file = "Train.csv",
    fileEncoding = 'UTF-8-BOM',
    stringsAsFactor = TRUE
)

str(main)
main$Reached.on.Time_Y.N <- as.factor(as.character(main$Reached.on.Time_Y.N))
levels(main$Reached.on.Time_Y.N)
main$Reached.on.Time_Y.N <- relevel(main$Reached.on.Time_Y.N, "1")

# 정소 도착 여부를 예측한 확률을 기록한 CSV파일을 생성하시오.

# 처음부터 8009건을 학습데이터로 생성

train <- main[1:8009, ]
test <- main[8010:nrow(main), ]

parts <- createDataPartition(
    train$Reached.on.Time_Y.N,
    p = 0.7,
    list = FALSE
)

str(train)
t.train <- train[parts, -12]
t.test <- train[parts, 12]

str(t.train)
str(t.test)
head(t.test)


t.train <- train[parts, ]
t.valid <- train[-parts, ]

str(t.train)
model <- preProcess(
    t.train[, -c(1, 12)],
    method = c("range")
)

s.train <- predict(
    model,
    t.train
)

s.valid <- predict(
    model,
    t.valid
)


str(s.train)

library(randomForest)
rf <- randomForest(
    Reached.on.Time_Y.N ~ . - ID, 
    s.train,
    do.trace = TRUE,
    ntree = 500
)

rf.p <- predict(
    rf,
    s.valid,
    type = 'class'
)

rf.p

caret::confusionMatrix(
    rf.p,
    s.valid$Reached.on.Time_Y.N
)

# Accuracy : 0.709

# svm

library(e1071)
sv <- svm(
    Reached.on.Time_Y.N ~ . - ID, 
    s.train
)

sv.p <- predict(
    sv,
    s.valid,
    type = 'class'
)

caret::confusionMatrix(
    sv.p,
    s.valid$Reached.on.Time_Y.N
)

# Accuracy : 0.7094

# 랜덤포레스트

# 확률 예측

set.seed(2010)
final <- randomForest::randomForest(
     Reached.on.Time_Y.N ~ . -ID,
     data = train,
     do.trace = TRUE,
     probability = TRUE
)

final.p <- predict(
    final,
    test,
    type = 'prob',
    probability = TRUE
)

final.p

model <- caret::preProcess(
    train[, -c(1, 12)],
    method = c("center", "scale")
)

t.train2 <- predict(
    model,
    train
)

head(t.train2)
