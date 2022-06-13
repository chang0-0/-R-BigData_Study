library(dplyr)
library(caret)
library(e1071)
library(randomForest)
library(scales)


setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/단원종합문제 2유형")
list.files()
main <- read.csv(
    file = "WA_Fn-UseC_-Telco-Customer-Churn.csv",
    fileEncoding = 'UTF-8-BOM',
    stringsAsFactor = TRUE
)

str(main)
head(main)

# 고객의 이탈 여부를 예측하고 csv 포맷으로 제출하시오.
# (이탈 : Yes , 유지 : "No")


main <- na.omit(main)


idx <- createDataPartition(
    main$Churn,
    p = 0.7
)


x_train <- main[idx$Resample1, -21]
x_test <- main[-idx$Resample1, -21]

y_train <- main[idx$Resample1, c(1,21)]
y_test <- main[-idx$Resample1, c(1,21)]

str(x_train)
idx <- createDataPartition(
    y_train$Churn, 
    p = 0.8
)

x_t <- x_train[idx$Resample1, ]
x_v <- x_train[-idx$Resample1, ]

y_t <- y_train[idx$Resample1, ]
y_v <- y_train[-idx$Resample1, ]

dim(x_t)
dim(x_v)

model <- preProcess(
    x_t, 
    method = c("range")
)


scaled_x_t <- predict(
    model,
    x_t
)

scaled_x_v <- predict(
    model,
    x_v
)



set.seed(21)
rf <- randomForest(
    y_t$Churn ~ . -customerID,
    scaled_x_t,
    ntree = 300,
    do.trace = T
)


predict_rf <- predict(
    rf,
    newdata = scaled_x_v
)

confusionMatrix(
    predict_rf,
    y_v$Churn,
    positive = 'Yes'
)
# Accuracy : 0.811

library(e1071)

svm.model <- svm(
    y_t$Churn ~ . -customerID,
    scaled_x_t,
)

predict.svm <- predict(
    svm.model,
    newdata = scaled_x_v
)

confusionMatrix(
    predict.svm,
    y_v$Churn,
    positive = 'Yes'
)

#  Accuracy : 0.8079

# 랜덤포레스트 선택



final.pred <- predict(
    rf, 
    newdata = x_test
)

levels(final.pred)
final.pred <- relevel(final.pred, "Yes") 
head(final.pred, 10)


write.csv(
    final.pred,
    "result0609.csv",
    row.names = FALSE
)

