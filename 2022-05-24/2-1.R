library(dplyr)
library(randomForest)
library(e1071)
library(caret)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/220520")
main.ds <- read.csv(
    file = "TravelInsurancePrediction.csv",
    stringsAsFactor = TRUE,
    encoding = "UTF-8"
)

head(main.ds)

# train을 1490개의 데이터로 설정
# 나머지는 test데이터

n <- nrow(maind.s)
train <- main.ds[1:1490, ]
test <- main.ds[1491:n, ]

library(scales)
percent(nrow(train) / nrow(main.ds))
# 75로 다시 분리

set.seed(11)
parts <- sample(
    nrow(main.ds),
    size = nrow(main.ds) * 0.75
)

train <- main.ds[parts, ]
test <- main.ds[-parts, ]
nrow(train)


train$TravelInsurance <- as.factor(train$TravelInsurance)


# train데이터를 사용해서 검증 데이터를 다시 생성 
set.seed(11)
idx <- sample(
    nrow(train),
    size = nrow(train) * 0.75
)

train_train <- train[idx, ]
train_valid <- train[-idx, ]

nrow(train_train)
nrow(train_valid)


# 스케일링
model <- preProcess(train_train[, -10], method = c("range"))
scaled_ds_train <- predict(model, train_train)
scaled_ds_valid <- predict(model, train_valid)
nrow(scaled_ds_train)
nrow(scaled_ds_valid)

# scale한 데이터는 scale을 했다 뿐이지, 전체 행의 길이는 변하지 않음
# 단 scale을 하기 위해서 기존의 train데이터에서 우리가 알려는 값
# 종속변수는 제외하고 sacle을 한다는 점

ncol(train_valid)
ncol(scaled_ds_valid)

head(train_valid)
head(scaled_ds_valid)


md_svm <- svm(
    TravelInsurance ~ .,
    data = scaled_ds_train,
    probability = TRUE
)

pred_svm <- predict(
    object = md_svm,
    newdata = scaled_ds_valid,
    probability = TRUE
)

caret::confusionMatrix(
    data = pred_svm,
    refer = scaled_ds_valid$TravelInsurance
)


set.seed(11)
md_rm <- randomForest(
    TravelInsurance ~ .,
    data = scaled_ds_train,
    ntree = 500,
    do.trace = T,
    probability = T
)

pred_rf <- predict(
    object = md_rm,
    newdata = scaled_ds_valid,
    probability = TRUE,
)

caret::confusionMatrix(
    data = pred_rf,
    refer = scaled_ds_valid$TravelInsurance
)


set.seed(11)
md_fit <- randomForest(
    TravelInsurance ~.,
    data = train,
    ntree = 500,
    do.trace = T,
    probability = TRUE
)


pred_fit <- predict(
    object = md_rm,
    newdata = test,
    probability = TRUE,
    type = "prob"
)

pred_fit


result <- data.frame(
    c(1:nrow(pred_fit)),
    pred_fit[, 2]
)

colnames(result) <- c("index", "y_pred")
head(result)

write.csv(result, "000.csv", row.names = FALSE)
result <- read.csv("000.csv" )
head(result)
result

head(result , 40)
