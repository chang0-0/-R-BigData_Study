library(dplyr)
library(caret)
library(ModelMetrics)
library(randomForest)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/최종모의고사3회")
main.ds <- read.csv(
    file = 'insurance.csv',
    fileEncoding = 'UTF-8-BOM',
    stringsAsFactor = TRUE
)

head(main.ds)
str(main.ds)
mean(main.ds$charges)
min(main.ds$charges)

set.seed(2022)
parts <- sample(
    c(1:nrow(main.ds)),
    size = nrow(main.ds) * 0.7
)

x_train <- main.ds[parts, ][-7]
x_test <- main.ds[-parts, ][-7]

y_train <- main.ds[parts, ][7]
y_test <- main.ds[-parts, ][7]

write.csv(x_train, "x_train.csv")
write.csv(x_test, "x_test.csv")

write.csv(y_train, "y_train.csv")
write.csv(y_test, "y_test.csv")

x_train <- read.csv("x_train.csv", fileEncoding = 'UTF-8-BOM', stringsAsFactor = TRUE)
x_test <- read.csv("x_test.csv", fileEncoding = 'UTF-8-BOM', stringsAsFactor = TRUE)
y_train <- read.csv("y_train.csv", fileEncoding = 'UTF-8-BOM', stringsAsFactor = TRUE)
y_test <- read.csv("y_test.csv", fileEncoding = 'UTF-8-BOM', stringsAsFactor = TRUE)

model_scale <- preProcess(
    x_train[, -1],
    method = c('range')
)

scaled_x_train <- predict(
    model_scale, x_train
)

scaled_x_test <- predict(
    model_scale, x_test
)

set.seed(2022)
idx <- sample(
    x = c(1:nrow(x_train)),
    size = nrow(x_train) * 0.8
)


x_t <- scaled_x_train[idx, ]
x_v <- scaled_x_train[-idx, ]

y_t <- y_train[idx, ]
y_v <- y_train[-idx, ]


trControl <- trainControl(method = 'cv', number = 5)

set.seed(2022)
model_glm <- train(
    x = x_t[, c(2:6)],
    y = y_t$charges,
    method = 'glm',
    trControl = trainControl(method = 'cv', number = 5)
)

model_rf <- train(
    x = x_t[, c(2:6)],
    y = y_t$charges,
    method = 'rf',
    trControl = trainControl(method = 'cv', number = 5)
)


getTrainPerf(model_glm)
getTrainPerf(model_rf)

set.seed(2022)
predict_glm <- predict(
    model_glm, 
    newdata = x_v[,c(2:6)]
)

set.seed(2022)
predict_rf <- predict(
    model_rf, 
    newdata = x_v[,c(2:6)]
)

predict_glm
predict_rf

rmse(y_v$charges, predict_glm)
rmse(y_v$charges, predict_rf)
rbind(rmse(y_v$charges, predict_glm), rmse(y_v$charges, predict_rf))

# rmse는 낮을 수록 정밀도가 높다.
# 따라서 randomForest를 선택

test_rf <- predict(
    model_rf, 
    newdata = x_test
)


result <- data.frame(
    x_test$X,
    test_rf
)

result

colnames(result) <- c("ID", "charges")
head(result)
write.csv(result, "result0603.csv", row.names = FALSE)

min(result$charges)
max(result$charges)
mean(result$charges)


# =========================================================

library(dplyr)
library(caret)
library(ModelMetrics)
library(randomForest)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/최종모의고사3회")
main.ds <- read.csv(
    file = 'insurance.csv',
    fileEncoding = 'UTF-8-BOM',
    stringsAsFactor = TRUE
)


set.seed(2022)
parts <- sample(
    c(1:nrow(main.ds)),
    size = nrow(main.ds) * 0.7
)

train <- main.ds[parts, ]
test <- main.ds[-parts, ]


set.seed(2022)
parts <- sample(
    c(1:nrow(train)),
    size = nrow(train) * 0.7
)

t.train <- train[parts, ]
t.valid <- train[-parts, ]

model <- preProcess(
    t.train,
    method = c('range')
)

sc.train <- predict(model, t.train) 
sc.valid <- predict(model, t.valid)

model_train <- lm(charges ~ ., sc.train)
model_train

step(model_train, direction = 'both')

form = formula(charges ~ age + bmi + smoker)

model_train <- lm(
    form,
    t.train
)

pred <- predict(
    model_train,
    newdata = test
)

rmse(test$charges, pred)

library(rpart)
rp <- rpart(
    charges ~ .,
    train,
    type = "anova"
)


pred <- predict(
    rp,
    test,
)
rmse(pred, test$charges)
rmse(pred)