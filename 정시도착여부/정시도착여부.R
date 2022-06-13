setwd("C:/Users/Samsung/Desktop/빅분기실기준비/정시도착여부")
list.files()

ds <- read.csv("Train.csv", 
    na.strings = c("", "NA"), 
    stringsAsFactor = TRUE, 
    fileEncoding = "UTF-8-BOM",
    header = TRUE
)

head(ds)
str(ds)
colSums(is.na(ds))

# 8009건을 학습 데이터로 정시도착 가능 여부예측 모델로 만들고, 

ds$Reached.on.Time_Y.N <- as.factor(as.character(ds$Reached.on.Time_Y.N))
n <- c(1:8009)
parts <- n
train <- ds[parts, ]
test <- ds[-parts, ]
scales::percent( nrow(train) / nrow(ds) )

str(train)
str(test)

library(randomForest)

ds.rf <- randomForest( Reached.on.Time_Y.N ~ . , data = train, ntree = 500, do.trace = TRUE )
ds.rf

ds.rf.pred <- predict( ds.rf, new_data = test , type="class" )
str(ds.rf.pred)



# train의 8009개의 데이터를 다시 검증을 위한 데이터로 분리
# 75:25의 데이터로 분리 75 = train, 25 = valid
set.seed(1)
idx <- caret::createDataPartition(train$Reached.on.Time_Y.N, p=0.75, list = FALSE)
ds_train <- train[idx, ]
ds_valid <- train[-idx, ]

nrow(ds_train)
nrow(ds_valid)

# 데이터 스케일링
model_prePro <- preProcess( 
    ds_train[, -12],
    method = c("range")
)

scaled_ds_train <- predict(model_prePro, ds_train)
scaled_ds_valid <- predict(model_prePro, ds_valid)

set.seed(1)
ds.rf <- randomForest( Reached.on.Time_Y.N ~ . ,
     data = scaled_ds_train, 
        ntree = 500,
     probability = TRUE)
ds.rf

ds.rf.pred <- predict( ds.rf, 
    newdata = scaled_ds_valid, 
    probability = TRUE,
    type = "response"
)

ds.rf.pred

caret::confusionMatrix(data = ds.rf.pred,
    refer = scaled_ds_valid$Reached.on.Time_Y.N
)

set.seed(2)
library(e1071)
md_svm <- svm(
    Reached.on.Time_Y.N ~ . , 
    data = scaled_ds_train, probability = TRUE
)

pred_svm <- predict(object = md_svm, newdata = scaled_ds_valid, probability = TRUE)
pred_svm

caret::confusionMatrix(
    data = pred_svm,
    refer = scaled_ds_valid$Reached.on.Time_Y.N
)

md_fit <- svm(
    Reached.on.Time_Y.N ~ . , 
    data = train,
    probability = TRUE,
)


# response = 0 ~ 1 사이의 확률을 구해줌
# class = 범주로 분류를 해줌
# prob = 
pred_fit <- predict(object = md_fit, 
    newdata = test, probability = TRUE, type = "prob"
)

pred_prob <- attr(pred_fit, "probablities")[,2]
pred_prob

result <- cbind(test$ID, pred_prob)
colnames(result) <- cbind("ID", "pred")
