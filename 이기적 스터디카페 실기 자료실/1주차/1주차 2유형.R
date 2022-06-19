library(dplyr)
library(caret)
library(tidyr)
library(ModelMetrics)
library(readr)
library(randomForest)


train <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/churn/train.csv',
    fileEncoding = 'UTF-8-BOM',
    na.strings = c("", " ", "NA", NA)
)

test <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/churn/test.csv',
    fileEncoding = 'UTF-8-BOM',
    na.strings = c("", " ", "NA", NA)
)

sub <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/churn/submission.csv',
    fileEncoding = 'UTF-8-BOM',
    na.strings = c("", " ", "NA", NA)
)


# 종속변수 Exited 예측
summary(train)
head(train)
summary(test)
head(test)
train <- train[,  -c(1,2,3)]
test <- test[,  -c(1, 3)]

#결측값 없음
colSums(is.na(train))
colSums(is.na(test))

train$Geography <- as.factor(train$Geography)
test$Geography <- as.factor(test$Geography)

train$Exited <- ifelse(train$Exited == 0, 'No', 'Yes')
train$Exited <- as.factor(train$Exited)

train$HasCrCard <- ifelse(train$HasCrCard == 1, 'Yes', 'No')
train$HasCrCard <- as.factor(train$HasCrCard)

test$HasCrCard <- ifelse(test$HasCrCard == 1, 'Yes', 'No')
test$HasCrCard <- as.factor(test$HasCrCard)

train$IsActiveMember <- ifelse(train$IsActiveMember == 1, 'Yes', 'No')
train$IsActiveMember <- as.factor(train$IsActiveMember)

test$IsActiveMember <- ifelse(test$IsActiveMember == 1, 'Yes', 'No')
test$IsActiveMember <- as.factor(test$IsActiveMember)

train$NumOfProducts <- as.factor(as.character(train$NumOfProducts))
test$NumOfProducts <- as.factor(as.character(test$NumOfProducts))

train$Gender <- as.factor(train$Gender)
test$Gender <- as.factor(test$Gender)

train$Tenure <- as.factor(train$Tenure)
test$Tenure <- as.factor(test$Tenure)



# 데이터 스케일링

model <- preProcess(
    train,
    method = c('range')
)

train <- predict(
    model,
    train
)


rf <- randomForest(
    Exited ~ .,
    train,
    ntree = 400,
    do.trace = TRUE
)

pred <- predict(
    object = rf,
    newdata = test
)


list <- sub
list <- ifelse(list == 0, 'No', 'Yes')
list <- as.factor(list)
levels(list) <- c('No', 'Yes')

auc(rf)

cm <- caret::confusionMatrix(
    pred,
    list,
    mode = 'everything'
)

f1score <- cm$byClass['F1']
print(f1score)
head(pred)

pred <- ifelse(pred == 'No', 0, 1)

result <- data.frame(
    test$CustomerId,
    pred
)

names(result) <- c('CustomerId', 'Exited')
setwd('C:/Users/Samsung/Desktop/빅분기실기준비/이기적 스터디카페 실기 자료실/1주차')
write.csv(result, 'result.csv', row.names = FALSE)