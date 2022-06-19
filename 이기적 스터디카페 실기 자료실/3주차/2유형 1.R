library(dplyr)
library(caret)
library(randomForest)
library(ModelMetrics)
library(tidyr)
library(readr)

# 데이터 설명 : 심장질환예측 target 컬럼

train <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/heart/train.csv',
    encoding = 'UTF-8',
    na.strings = c('', ' ', 'NA', NA)
)

test <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/heart/test.csv',
    encoding = 'UTF-8',
    na.strings = c('', ' ', 'NA', NA)
)

sub <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/heart/submission.csv',
    encoding = 'UTF-8',
    na.strings = c('', ' ', 'NA', NA)
)

# 결측값 없음
colSums(is.na(train))

train$sex <- as.factor(ifelse(train$sex == 1 , 'male', 'female'))
test$sex <- as.factor(ifelse(test$sex == 1 , 'male', 'female'))

train$exang <- as.factor(ifelse(train$exang == 1 , 'yes', 'no'))
test$exang <- as.factor(ifelse(test$exang == 1 , 'yes', 'no'))

train$fbs <- as.factor(ifelse(train$fbs == 1 , 'true', 'false'))
test$fbs <- as.factor(ifelse(test$fbs == 1 , 'true', 'false'))

train$thal <- as.factor(ifelse(train$thal == 1 , 'normal', ifelse(train$thal == 2, 'fixed', 'reversable') ))
test$thal <- as.factor(ifelse(test$thal == 1 , 'normal', ifelse(test$thal == 2, 'fixed', 'reversable') ))

train$restecg <- as.factor(train$restecg)
test$restecg <- as.factor(test$restecg)

train$cp <- as.factor(train$cp)
test$cp <- as.factor(test$cp)

train$slope <- as.factor(train$slope)
test$slope <- as.factor(test$slope)

train$ca <- as.factor(train$ca)
test$ca <- as.factor(test$ca)

train$target <- as.factor(train$target)


str(train)
model <- preProcess(
    train[, -c(1)],
    method = c('range')
)

train <- predict(
    model,
    train
)

model <- preProcess(
    test[, -c(1)],
    method = c('range')
)

test <- predict(
    model,
    test
)


rf <- randomForest(
    target ~ .,
    train,
    do.trace = TRUE,
    ntree = 400
)

print(auc(rf))


pred <- predict(
    rf,
    newdata = test
)

head(sub)
caret::confusionMatrix(
    pred,
    sub$X0 ,
    positive = '1'
)

result <- data.frame(
    pred
)

names(result) <- c('target')
head(result)


pred2 <- predict(
    rf,
    newdata = test,
    type = 'prob',
    probability = TRUE
)

head(pred2)
