library(dplyr)
library(caret)
library(ModelMetrics)
library(randomForest)
setwd("C:/Users/Samsung/Desktop/빅분기실기준비/데이터마님 빅분기/2유형 연습1")


x_train <- read.csv("https://raw.githubusercontent.com/Datamanim/datarepo/main/churnk/X_train.csv")
x_test <- read.csv("https://raw.githubusercontent.com/Datamanim/datarepo/main/churnk/X_test.csv")
y_train <- read.csv("https://raw.githubusercontent.com/Datamanim/datarepo/main/churnk/y_train.csv")


nrow(x_train)
nrow(x_test)
nrow(y_train)
# Exited 컬럼을 예측
head(y_train$Exited, 100)


summary(x_train)
summary(y_train)
colSums(is.na(x_train))
colSums(is.na(x_test))
colSums(is.na(y_train))

full <- merge(x_train, y_train, by = "CustomerId" )
nrow(full)
summary(full)
colSums(is.na(full))
str(full)

full <- subset(full, select = -c(Surname, CustomerId))
full$Gender <- as.factor(full$Gender)
full$Geography <- as.factor(full$Geography)
full$Exited <- as.factor(as.character(full$Exited))
full$HasCrCard <- as.factor(as.character(full$HasCrCard))
full$IsActiveMember <- as.factor(as.character(full$IsActiveMember))
full$NumOfProducts <- as.factor(as.character(full$NumOfProducts))


x_test <- subset(x_test, select = -c(Surname))
x_test$Gender <- as.factor(x_test$Gender)
x_test$Geography <- as.factor(x_test$Geography)
x_test$HasCrCard <- as.factor(as.character(x_test$HasCrCard))
x_test$IsActiveMember <- as.factor(as.character(x_test$IsActiveMember))
x_test$NumOfProducts <- as.factor(as.character(x_test$NumOfProducts))


model <- preProcess(
    full,
    method = c('range')
)
full <- predict(model, full)
summary(full)

model <- preProcess(
    x_test[, c(-1)],
    method = c('range')
)
x_test <- predict(model, x_test)
summary(x_test)


rf <- randomForest(
    Exited ~ .,
    data = full,
    do.trace = TRUE,
    ntree = 400
)

pred <- predict(
    object = rf,
    newdata = x_test,
    type = 'class'
)

head(pred, 40)

result <- data.frame(
    x_test$CustomerId,
    pred
)
names(result) <- c("CustomerId", "Exited")
head(result)

write.csv(result, "result.csv", row.names = F)
Rtest <- read.csv("result.csv")
head(Rtest)
nrow(Rtest)