library(dplyr)
library(caret)
library(randomForest)
library(lubridate)
library(ModelMetrics)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/기출2회")
list.files()

main <- read.csv(
    file = "Train.csv",
    fileEncoding = "UTF-8-BOM",
    stringsAsFactor = TRUE
)


# train 데이터와 정답이 없는 label 데이터 제공
# 정시도착 여부를 예측한 확률을 기록한 CSV를 생성하시오.

x_train <- main[1:8009, -1]
x_test <- main[8010:nrow(main), -12]

str(x_train)
head(x_train, 10)
x_train$Reached.on.Time_Y.N <- as.factor(as.character(x_train$Reached.on.Time_Y.N))
levels(x_train$Reached.on.Time_Y.N)
# 정시 도착 = 0, 정시도착 하지 않음 = 1


model <- preProcess(
    x_train[, -c(1, 12)],
    method = c("range")
)
x_train <- predict(model, x_train)

model <- preProcess(
    x_test[,-1],
    method = c("range")
)
x_test <- predict(model, x_test)

summary(x_train)
summary(x_test)

final <- svm(
    Reached.on.Time_Y.N ~ . ,
    data = x_train,
    probability = TRUE
)

pred <- predict(
    final,
    newdata = x_test,
    probability = TRUE,
    type = 'prob'
)

newpred <- attr(pred, "probabilities")
head(newpred)
newpred <- newpred[,2]

length(newpred)

for(i in 1:length(newpred)) {
    newpred[i] <- round(newpred[i], 6)
}

result <- data.frame(
    x_test$ID,
    newpred
)

head(result)
names(result) <- c("ID", "pred")

write.csv(result, "0613.csv", row.names = FALSE)
result_test <- read.csv(
    "0613.csv"
)

head(result_test)
