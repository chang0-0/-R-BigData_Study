library(mlbench)
library(randomForest)
library(ModelMetrics)
library(C50)
library(Epi)

data("PimaIndiansDiabetes2")
ds <- PimaIndiansDiabetes2
ds <- na.omit(ds)

# 난수 시드 설정
set.seed(2011)
train.idx <- sample(1:nrow(ds), size=nrow(ds) * 0.8)

train <- ds[train.idx, ]
test <- ds[-train.idx, ]

ds.forest <- randomForest(diabetes ~ . , data = train, ntree = 500)
pred <- predict(ds.forest, newdata = test, type = "prob")
head(pred)

auc(actual = test$diabetes, predicted = as.factor(pred))

#ROC 커브를 통해서 auc 확인
str(pred)

# ROC커버를 쓰려면 predict 예측값을 "prob" 타입으로 설정해야됨.
result <- ROC(test = pred[, 2], stat = test$diabetes)

