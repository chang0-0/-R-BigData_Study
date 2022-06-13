library(dplyr)
library(e1071)
library(randomForest)
library(caret)
library(knitr)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/기출3회")
main.ds <- read.csv(
    "TravelInsurancePrediction.csv",
    encoding = "UTF-8",
    stringsAsFactor = TRUE
)

tot_row <- nrow(main.ds)
X_train <- main.ds[1:1490, ]
X_test <- main.ds[1491:tot_row, -10]


X_train$TravelInsurance <- as.factor(
    X_train$TravelInsurance
)

# 결측값 없음
set.seed(13579)
idx <- createDataPartition(
    X_train$TravelInsurance,
    p = 0.8,
    list = FALSE
)

ds_train <- X_train[idx, ]
ds_valid <- X_train[-idx, ]


ds_train$TravelInsurance <- as.factor(
    ds_train$TravelInsurance
)
ds_valid$TravelInsurance <- as.factor(
    ds_valid$TravelInsurance
)


model <- preProcess(
    ds_train[, -10],
    method = c("range")

)

scaled_train <- predict(model, ds_train)
scaled_valid <- predict(model, ds_valid)


md_svm <- svm(
    TravelInsurance ~ .,
    scaled_train,
    probability = TRUE,
)

pred_svm <- predict(
    object = md_svm,
    newdata = scaled_valid,
    type = 'class',
    probability = TRUE
)

caret::confusionMatrix(
    data = pred_svm,
    refer = scaled_valid$TravelInsurance,
)

# Accuracy : 0.7946

md_rf <- randomForest(
    TravelInsurance ~ .,
    scaled_train,
    probability = TRUE,
    ntree = 500
)

pred_rf <- predict(
    object = md_rf,
    newdata = scaled_valid,
    type = 'prob',
    probability = TRUE
)

caret::confusionMatrix(
    data = pred_rf,
    refer = scaled_valid$TravelInsurance,
)

# Accuracy : 0.8148

# randomForest선택 결과
set.seed(10)
md_fit <- randomForest(
    TravelInsurance ~ .,
    scaled_train,
    ntree = 500,
    do.trace = TRUE,
    probability = TRUE,
)

pred_fit <- predict(
    md_fit,
    newdata = X_test,
    probability = TRUE,
    type = 'prob',
)

pred_fit[,2]

X_result <- data.frame(
    c(1:nrow(X_test)),
    pred_fit[, 2]
)

names(X_result ) <- c("index", "y_pred")

write.csv(X_result, "result2.csv", row.names = FALSE)
