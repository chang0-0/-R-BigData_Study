install.packages("xgboost")
library(xgboost)
library(mlbench)
library(caret)

data("PimaIndiansDiabetes2")
ds <- PimaIndiansDiabetes2
ds <- na.omit(ds)

set.seed(20)
parts <- sample(1:nrow(ds) , size = nrow(ds) * 0.7 )
parts

train <- ds[parts, ]
test <- ds[-parts, ]

train.label <- as.integer(train$diabetes)-1
mat_train.data <- as.matrix(train[, -9])
mat_test.data <- as.matrix(test[, -9])

xgb.train <- xgb.DMatrix(
    data = mat_train.data, 
    label = train.label)

xgb.test <- xgb.DMatrix(
    data = mat_test.data)

param_list <- list(
    booster = "gbtree",
    eta = 0.001,
    max_depth = 10,
    gamma = 5,
    subsampe = 0.8,
    colsample_bytree = 0.8,
    objective = "binary:logistic",
    eval_metric = "auc")

md.xgb <- xgb.train(
    params = param_list,
    data = xgb.train,
    nrounds = 200,
    early_stopping_rounds = 10,
    watchlist = list(val1 = xgb.train), 
    verbose = 1
)

xgb.pred <- predict(md.xgb, newdata = xgb.test)
xgb.pred

xgb.pred2 <- factor(ifelse(xgb.pred >= 0.5, 1, 0), levels = c(0, 1),
    labels = c("neg", "pos")
) 

# 위와 같은 facor 변경 법

xgb.pred2 <- ifelse(
    xgb.pred >= 0.5,
    xgb.pred <- "pos",
    xgb.pred <- "neg"
)
xgb.pred2 <- as.factor(xgb.pred2)


caret::confusionMatrix(
    xgb.pred2, reference = test$diabetes, 
    positive = "pos"
)

## 랜덤포레스트 예제
library(randomForest)
md.rf <- randomForest(diabetes ~ . , data = train, ntree =  100, proximity = TRUE, importance = TRUE)
md.rf

md.rf.pred <- predict(md.rf, newdata = test)

caret::confusionMatrix(
    as.factor(md.rf.pred), 
    test$diabetes,
    positive = "pos"
)




