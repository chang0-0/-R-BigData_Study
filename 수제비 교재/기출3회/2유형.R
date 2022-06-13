library(dplyr)
library(e1071)
library(randomForest)
library(caret)
library(adabag)
install.packages("adabag")

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/기출3회")
main.ds <- read.csv(
    "TravelInsurancePrediction.csv",
    encoding = "UTF-8",
    stringsAsFactor = TRUE
)

head(main.ds)
str(main.ds)
main.ds$TravelInsurance <- as.factor(main.ds$TravelInsurance)
head(main.ds, 10)
colSums(is.na(main.ds))

# 여행 보험 가입여부예측모델을 만들고,
# test.csv로 여행보험 패키지 가입여부를 예측하는 결과를 예시파일로 만드시오

set.seed(10)
parts <- sample(
    1:nrow(main.ds),
    size = nrow(main.ds) * 0.7
)

train <- main.ds[parts, ]
test <- main.ds[-parts, -10]

# valid 데이터 생성


parts <- sample(
    1:nrow(train),
    size = nrow(train) * 0.7
)

t.train <- train[parts, ]
t.valid <- train[-parts, ]


# 데이터 정규화를 위해 스케일링
model_prePro <- preProcess(
    t.train[, -10],
    method = c("range")
)

model_prePro
scaled_train <- predict(
    model_prePro, 
    t.train
)


str(t.train)

set.seed(10)
md.rf <- randomForest(
    TravelInsurance ~ .,
    t.train,
    do.trace = TRUE,
    ntree = 500,
    probability = TRUE
)

md.rf.pred <- predict(
    object = md.rf,
    t.valid, 
    probability = TRUE, 
    type = 'class'
)

caret::confusionMatrix(
    data = md.rf.pred,
    refer = t.valid$TravelInsurance
)

# Accuracy : 0.8421


md.bag <- bagging(
    TravelInsurance ~ .,
    data = t.train, 
    mfinal = 100,
    do.trace = TRUE,
)

md.bag.pred <- predict(
    md.bag,
    newdata = t.valid,
)

md.bag.pred$formula
md.bag.pred$class

temp <- md.bag.pred$class
temp <- as.factor(temp)


t.valid$TravelInsurance <- relevel(t.valid$TravelInsurance, "1")

caret::confusionMatrix(
    data = temp,
    refer = t.valid$TravelInsurance
)

# Accuracy : 0.8541

set.seed(10)
md.svm <- svm(
    TravelInsurance ~ .,
    t.train,
    probability = TRUE
)

md.svm.pred <- predict(
    object = md.svm,
    t.valid, 
    probability = TRUE,
    type = 'class'
)

caret::confusionMatrix(
    data = md.svm.pred,
    refer = t.valid$TravelInsurance
)

# Accuracy : 0.8397
# SVM이 미세하게 더 높으므로 SVM채택


last.svm <- svm(
    TravelInsurance ~ .,
    train,
    probability = TRUE
)

last.svm.pred <- predict(
    object = last.svm,
    test, 
    probability = TRUE,
    type = 'prob'
)

last.svm.pred
head( attr(last.svm.pred, "probabilities") )

temp <- attr(last.svm.pred, "probabilities")

temp <- temp[,1]
n <- c(1:length(temp))

result <- data.frame(
    n,
    temp
)

names(result) <- c("index", "y_pred")
write.csv(result, "result.csv", row.names = FALSE)
result2 <- read.csv("result.csv")
head(result2)
