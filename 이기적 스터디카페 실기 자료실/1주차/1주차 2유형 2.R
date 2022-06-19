library(dplyr)
library(caret)
library(randomForest)
library(e1071)
library(tidyr)
library(ModelMetrics)


# 종속변수 발생 여부 예측 (종속변수 diagnosis : B(양성), M(악성))

train <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/cancer/train.csv',
    fileEncoding = 'UTF-8-BOM',
    na.strings = c("", " ", "NA", NA)
)

test <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/cancer/test.csv',
    fileEncoding = 'UTF-8-BOM',
    na.strings = c("", " ", "NA", NA)
)

sub <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/cancer/submission.csv',
    fileEncoding = 'UTF-8-BOM',
    na.strings = c("", " ", "NA", NA)
)


train <- train[, -c(1)]
train$diagnosis <- as.factor(train$diagnosis)


#  데이터 검증 테스트

set.seed(2108)
parts <- sample(
    1:nrow(train),
    size = nrow(train) * 0.7
)

t.train <- train[parts, ]
t.valid <- train[-parts, ]

model <- preProcess(
    t.train[, -1],
    method = c('range')
)

sd.train <- predict(
    model,
    t.train
)

sd.valid <- predict(
    model,
    t.valid
)

rf <- randomForest(
    diagnosis ~ .,
    sd.train,
    ntree = 400,
    do.trace = TRUE
)

rf.p <- predict(
    rf,
    newdata = sd.valid,
)

caret::confusionMatrix(
    rf.p,
    sd.valid$diagnosis,
    mode = 'everything'
)

# Accuracy : 0.9708


sv <- svm(
    diagnosis ~ .,
    sd.train,
)

sv.p <- predict(
    sv,
    newdata = sd.valid,
)

caret::confusionMatrix(
    sv.p,
    sd.valid$diagnosis,
    mode = 'everything'
)

#  Accuracy : 0.9927

# svm 승리

last <- svm(
    diagnosis ~ .,
    train,
)

pred <- predict(
    last,
    newdata = test
)

result <- data.frame(
    test$id,
    pred
)


names(result) <- c('id', 'diagnosis')
head(result)