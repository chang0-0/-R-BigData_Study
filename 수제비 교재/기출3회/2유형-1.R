library(caret)
library(randomForest)
library(e1071)
library(rpart)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/기출3회")
list.files()
main.ds <- read.csv(
    file = "TravelInsurancePrediction.csv",
    encoding = 'UTF-8',
    stringsAsFactor = TRUE
)


summary(main.ds)
colSums(is.na(main.ds))
str(main.ds)

set.seed(2011)
parts <- sample(
    nrow(main.ds),
    size = nrow(main.ds) * 0.7
)

train <- main.ds[parts, ]
test <- main.ds[-parts, ]
nrow(train)
nrow(test)

# valid 데이터 생성
set.seed(2011)
parts <- sample(
    nrow(train),
    size = nrow(train) * 0.7
)

t.train <- train[parts, ]
t.valid <- train[-parts, ]
str(t.train)

forest <- randomForest(
    TravelInsurance ~ .,
    data = t.train,
    do.trace = T,
    ntree = 500,
)

forest.pred <- predict(
    object = forest,
    newdata = t.valid,
    type = 'class'
)

forest.pred <- ifelse(
    forest.pred >= 0.5,
    forest.pred <- 1,
    forest.pred <- 0
)
forest.pred <- as.factor(as.character(forest.pred ))
t.valid$TravelInsurance <- as.factor(as.character(t.valid$TravelInsurance))

caret::confusionMatrix(
    data = forest.pred,
    refer = t.valid$TravelInsurance,
)
# Accuracy : 0.823
mean(t.valid$TravelInsurance == forest.pred)

svm_model <- svm(
    TravelInsurance ~ .,
    data = t.train,
)

svm.pred <- predict(
    object = svm_model,
    newdata = t.valid,
    type = 'prob'
)

svm.pred <- ifelse(
    svm.pred >= 0.5,
    svm.pred <- 1,
    svm.pred <- 0
)
svm.pred <- as.factor(as.character(svm.pred ))
caret::confusionMatrix(
    data = svm.pred,
    refer = t.valid$TravelInsurance,
)

# Accuracy : 0.7799
# 결론 : 랜덤포레스트가 정확도가 더 높음


# 결과값은 확률 형태로 출력


final.forest <- randomForest(
    TravelInsurance ~ .,
    data = train,
    do.trace = T,
    ntree = 500,
)

final.forest.pred <- predict(
    object = final.forest,
    newdata = test,
    probility = TRUE,
)


# 결과 제출 
# index값, y_pred값으로 컬럼이름 설정 2가지
result <- data.frame(
    c(1:nrow(test)),
    round(final.forest.pred, 6)
)

names(result) <- c("index", "y_pred")
write.csv(result, "result.csv", row.names = FALSE)

# =========================교재 부분대로 수정=========================
# =========================교재 부분대로 수정=========================
# =========================교재 부분대로 수정=========================
# =========================교재 부분대로 수정=========================
# =========================교재 부분대로 수정=========================


library(caret)
library(randomForest)
library(e1071)
library(rpart)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/기출3회")
list.files()
main.ds <- read.csv(
    file = "TravelInsurancePrediction.csv",
    encoding = 'UTF-8',
    stringsAsFactor = TRUE
)

main.ds$TravelInsurance <- as.factor(as.character(main.ds$TravelInsurance))

summary(main.ds)
colSums(is.na(main.ds))
str(main.ds)

set.seed(2011)
parts <- sample(
    nrow(main.ds),
    size = nrow(main.ds) * 0.7
)

train <- main.ds[parts, ]
test <- main.ds[-parts, ]
nrow(train)
nrow(test)

set.seed(2011)
parts <- sample(
    nrow(train),
    size = nrow(train) * 0.7
)

t.train <- train[parts, ]
t.valid <- train[-parts, ]

model_prePro <- preProcess(
    t.train[,-10],
    method = c("range")
)

sc_train <- predict(
    model_prePro,
    t.train
)

sc_valid <- predict(
    model_prePro,
    t.valid
)

# randomForest
set.seed(2011)
forest <- randomForest(
    TravelInsurance ~ .,
    data = sc_train,
    do.trace = T,
    ntree = 500,
    probability = TRUE,
)

forest.pred <- predict(
    object = forest,
    newdata = sc_valid,
    type = 'class',
)

labels(forest.pred)
levels(forest.pred)


caret::confusionMatrix(
    data = forest.pred,
    refer = sc_valid$TravelInsurance,
    positive ='1'
)
# Accuracy : 0.8158
mean(sc_valid$TravelInsurance == forest.pred)

# SVM
set.seed(2011)
svm_model <- svm(
    TravelInsurance ~ .,
    data = sc_train,
    probaility = TRUE
)

svm.pred <- predict(
    object = svm_model,
    newdata = sc_valid,
    probaility = TRUE,
)

head(svm.pred, 40)
levels(svm.pred)


# 둘다 positivie를 1로 바꿔줌
svm.pred <- relevel(svm.pred, ref = '1')
head(svm.pred)
levels(svm.pred)

head(t.valid$TravelInsurance, 40)
levels(t.valid$TravelInsurance)

svm.pred <- relevel(svm.pred, ref = '1')
head(svm.pred, 40)
levels(svm.pred)

t.valid$TravelInsurance <- relevel(t.valid$TravelInsurance , ref = '1')
head(t.valid$TravelInsurance, 40)
levels(t.valid$TravelInsurance)

caret::confusionMatrix(
    data = svm.pred,
    refer = t.valid$TravelInsurance,
)
# Accuracy : 0.7847


# randomForest로 결론
set.seed(2011)
final_forest <- randomForest(
    TravelInsurance ~ .,
    data = train,
    do.trace = T,
    ntree = 500,
    probability = TRUE,
)

final_forest.pred <- predict(
    object = final_forest,
    newdata = test,
    type = 'prob',
)


result <- data.frame(
    c(1:nrow(test)),
    final_forest.pred[, 2]
)

names(result) <- c("index", "y_pred")
write.csv(result, "result0531.csv", row.names = FALSE)

temp <- final_forest.pred[, 2] 

temp <- ifelse( 
     temp >= 0.5,
     temp <- 1,
     temp <- 0
)

caret::confusionMatrix(
    temp,
    test$TravelInsurance,
    positive = "1"
)
