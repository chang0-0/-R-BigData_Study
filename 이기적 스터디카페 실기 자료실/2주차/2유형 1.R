library(dplyr)
library(randomForest)
library(caret)
library(ModelMetrics)
library(tidyr)
library(readr)

# explain
# 2018년도 성인의 건강검진 데이터로부터 흡연상태 예측 (흡연상태 1- 흡연, 0-비흡연 )

train <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/smoke/train.csv',
    encoding = 'UTF-8',
    na.strings = c("", " ", "NA", NA),
    stringsAsFactor = TRUE
)

nrow(train)

test <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/smoke/test.csv',
    encoding = 'UTF-8',
    na.strings = c("", " ", "NA", NA),
    stringsAsFactor = TRUE
)

sub <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/smoke/submission.csv',
    encoding = 'UTF-8-BOM',
    na.strings = c("", " ", "NA", NA),
    stringsAsFactor = TRUE
)

levels(train$구강검진수검여부) <- c('Y', 'N')
levels(test$구강검진수검여부) <- c('Y', 'N')

train$흡연상태 <- as.factor(train$흡연상태)
head(train$흡연상태)

# positive = 비흡연 0
train$청력.좌. <- as.factor(train$청력.좌.)
train$청력.우. <- as.factor(train$청력.우.)

test$청력.좌. <- as.factor(test$청력.좌.)
test$청력.우. <- as.factor(test$청력.우.)

train$연령대코드.5세단위. <- as.factor(train$연령대코드.5세단위.)
test$연령대코드.5세단위. <- as.factor(test$연령대코드.5세단위.)


head(train$흡연상태)

model <- preProcess(
    train[,  -c(6) ],
    method = c('range')
)

t.train <- predict(
    model,
    train
)

rf <- randomForest(
    흡연상태 ~ .,
    t.train,
    do.trace = TRUE,
    ntree = 400
)

pred <- predict(
    rf,
    newdata = test
)

sub$X0 <- as.factor(sub$X0)
levels(sub$X0) <- c('0', '1')

cm <- caret::confusionMatrix(
    pred,
    sub$X0,
    mode = 'everything'
)

print(cm$byClass['F1'])


result <- data.frame(
    pred
)

write.csv(result, 'result.csv', row.names = F)



# 데이터 검증


train <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/smoke/train.csv',
    encoding = 'UTF-8',
    na.strings = c("", " ", "NA", NA),
    stringsAsFactor = TRUE
)

test <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/smoke/test.csv',
    encoding = 'UTF-8',
    na.strings = c("", " ", "NA", NA),
    stringsAsFactor = TRUE
)

sub <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/smoke/submission.csv',
    encoding = 'UTF-8-BOM',
    na.strings = c("", " ", "NA", NA),
    stringsAsFactor = TRUE
)

levels(train$구강검진수검여부) <- c('Y', 'N')
levels(test$구강검진수검여부) <- c('Y', 'N')

train$흡연상태 <- as.factor(train$흡연상태)
head(train$흡연상태)

# positive = 비흡연 0
train$청력.좌. <- as.factor(train$청력.좌.)
train$청력.우. <- as.factor(train$청력.우.)

test$청력.좌. <- as.factor(test$청력.좌.)
test$청력.우. <- as.factor(test$청력.우.)

train$연령대코드.5세단위. <- as.factor(train$연령대코드.5세단위.)
test$연령대코드.5세단위. <- as.factor(test$연령대코드.5세단위.)

set.seed(2109)
parts <- sample(
    1:nrow(train),
    size = nrow(train) * 0.7
)

t.train <- train[parts, ]
t.valid <- train[-parts, ]

model <- preProcess(
    t.train,
    method = c('range')
)

ds.train <- predict(
    model,
    t.train
)

ds.valid <- predict(
    model,
    t.valid
)


rf <- randomForest(
    흡연상태 ~ .,
    ds.train,
    do.trace = TRUE,
    ntree = 300
)


rf.p <- predict(
    rf,
    newdata = ds.valid
)

caret::confusionMatrix(
    rf.p,
    ds.valid$흡연상태,
    mode = 'everything'
)

#  Accuracy : 0.7609
#  F1 : 0.8057

# SVM
library(e1071)
sv <- svm(
    흡연상태 ~ .,
    ds.train,
    do.trace = TRUE
)

sv

sv.p <- predict(
    rf,
    newdata = ds.valid
)

caret::confusionMatrix(
    sv.p,
    ds.valid$흡연상태,
    mode = 'everything'
)

#  Accuracy : 0.7612
# F1 : 0.8059

# 랜덤포레스트와 비슷한 결과를 가짐
