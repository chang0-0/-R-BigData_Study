library(caret)
library(tidyr)
library(dplyr)
library(randomForest)
library(rpary)
library(e1071)
library(rpart)
library(pROC)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/기출2회")

main <- read.csv(
    file = "Train.csv",
    fileEncoding = 'UTF-8-BOM',
    stringsAsFactor = TRUE
)

str(main)
head(main)
summary(main)
colSums(is.na(main))

main$Reached.on.Time_Y.N <- as.factor(main$Reached.on.Time_Y.N)
main$Reached.on.Time_Y.N <- relevel(main$Reached.on.Time_Y.N, "1")

n <- nrow(main)

x_train <- main[1:8009, ]
x_test <- main[8010:n, -12]

scales::percent(nrow(x_train) / nrow(main))

# 데이터 전처리 할 때, 결과를 예측할 컬럼은 제외

parts <- createDataPartition(
    x_train$Reached.on.Time_Y.N,
    p = 0.73
)

t.train <- x_train[parts$Resample1, ]
t.valid <- x_train[-parts$Resample1, ]

model <- preProcess(
    t.train[, -12],
    method = c("range")
)

sc.train <- predict(
    model, t.train
)

sc.valid <- predict(
    model, t.valid
)

summary(sc.valid)

# 랜덤포레스트

rf <- randomForest(
    Reached.on.Time_Y.N ~ .-ID,
    sc.train,
    do.trace = T,
    ntree = 500
)

rf.pred <- predict(
    rf,
    newdata = sc.valid,
    method = 'prob',
    probability = TRUE
)

head(rf.pred)

mean(rf.pred == sc.valid$Reached.on.Time_Y.N)
# Accuracy : 0.7186
caret::confusionMatrix(
    rf.pred,
    sc.valid$Reached.on.Time_Y.N
)

rf.pred <- as.numeric(as.character(rf.pred))

rf.roc <- pROC::roc(
    sc.valid$Reached.on.Time_Y.N,
    rf.pred
)

# Area under the curve: 0.706
summary(rf.roc)
rf.roc

# SVM
# Suppport vector Machine

svm.model <- svm(
    Reached.on.Time_Y.N ~ .-ID,
    sc.train,
)

svm.pred <- predict(
    svm.model,
    newdata = sc.valid,
    type = 'response',
)

mean(svm.pred == sc.valid$Reached.on.Time_Y.N)
#  Accuracy : 0.7075
caret::confusionMatrix(
    svm.pred,
    sc.valid$Reached.on.Time_Y.N
)

svm.pred <- as.numeric(as.character(svm.pred))

svm.roc <- pROC::roc(
    sc.valid$Reached.on.Time_Y.N,
    svm.pred,
    levels = c(1,0),
    direction = ">"
)

# Area under the curve: 0.6993
svm.roc

# decision Tree
rpart.model <- rpart(
    Reached.on.Time_Y.N ~ .-ID,
    sc.train,
    method = 'class'
)

rpart.pred <- predict(
    rpart.model,
    data = sc.valid,
    method = 'prob',
    probability = TRUE
)


mean(rpart.pred == sc.valid$Reached.on.Time_Y.N)
# Accuracy : 0.7325
caret::confusionMatrix(
    rpart.pred,
    sc.valid$Reached.on.Time_Y.N
)

rpart.pred <- as.numeric(as.character(rpart.pred))

# Area under the curve: 0.7524
rpart.roc <- pROC::roc(
    sc.valid$Reached.on.Time_Y.N,
    rpart.pred,
    levels = c(1,0),
    direction = ">"
)

# 최종

final <- randomForest(
    Reached.on.Time_Y.N ~ .-ID,
    x_train,
    do.trace = T,
    ntree = 500,
    keep.forest = TRUE
)

final.pred <- predict(
    final,
    newdata = x_test,
    probability = TRUE
)

head(final.pred )
head(attr(final.pred , "probabilities"))


final2 <- svm(
    Reached.on.Time_Y.N ~ .-ID,
    x_train,
    probability = TRUE
)

final2.pred <- predict(
    final2,
    x_test,
    probability = TRUE
)


head( attr(final2.pred, "probabilities"))

final2list <- attr(final2.pred, "probabilities")[,1]


















final.list <- final.pred[,1]
head(final.list)

idlist <- x_test$ID
result <- data.frame(
    idlist,
    final.list
)

head(result)
names(result) <- c("ID", "pred")

write.csv(result, "result0605.csv", row.names = FALSE)
result.test <- read.csv(
    "result0605.csv"
)

head(result.test)
