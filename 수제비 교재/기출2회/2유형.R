library(dplyr)
library(caret)
library(e1071)
library(randomForest)
library(scales)


setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/기출2회")
list.files()
main.ds <- read.csv(
    file = "Train.csv",
    encoding = "UTF-8",
    stringsAsFactor = TRUE
)

tail(main.ds)

head(main.ds, 20)
str(main.ds)
summary(main.ds)
colSums(is.na(main.ds))
main.ds$Reached.on.Time_Y.N <- as.factor(as.character(main.ds$Reached.on.Time_Y.N))

train <- main.ds[1:8009, ]
test <- main.ds[8010:nrow(main.ds), -12]
str(test)
str(train)

scales::percent( nrow(train) / nrow(main.ds))

set.seed(100)
parts <- sample(
    1:nrow(train),
    size = nrow(train) * 0.73
)

t.train <- train[parts, ]
t.valid <- train[-parts, ]
scales::percent( nrow(t.train) / nrow(train))

str(t.train)

md.rf <- randomForest(
    Reached.on.Time_Y.N ~ . - X.U.FEFF.ID,
    t.train,
    do.trace = TRUE,
    ntree = 500,
    probability = TRUE,
    type = 'class',
)

rf.pred <- predict(
    md.rf,
    newdata = t.valid,
    type = 'response',
    probability = TRUE,
)

confusionMatrix(
    rf.pred,
    t.valid$Reached.on.Time_Y.N,
)

# Accuracy : 0.6995

last_rf <- randomForest(
    Reached.on.Time_Y.N ~ . -X.U.FEFF.ID,
    train,
    do.trace = TRUE,
    ntree = 500,
    probability = TRUE,
)

last_pred <- predict(
    last_rf,
    newdata = test,
    type = 'prob',
    probability = TRUE
)



head(last_pred)
str(train)
# 0이 정시도착

list <- last_pred[ ,1]

head(list)

result <- data.frame(
    test$ X.U.FEFF.ID,
    list
)

head(result)

names(result) <- c("ID", "pred")
write.csv(result, "result.csv", row.names = FALSE)
result.test <- read.csv("result.csv")
head(result.test)
