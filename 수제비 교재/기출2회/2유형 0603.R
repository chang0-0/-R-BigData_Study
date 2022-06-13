library(caret)
library(dplyr)
library(randomForest)
library(e1071)
library(scales)
library(class)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/기출2회")
list.files()

main.ds <- read.csv(
    file = "Train.csv",
    fileEncoding = 'UTF-8-BOM',
    stringsAsFactor = TRUE,
)

head(main.ds)
summary(main.ds)
str(main.ds)

main.ds$Reached.on.Time_Y.N <- as.factor(as.character(main.ds$Reached.on.Time_Y.N))

# 정시도착 여부 컬럼 facotr형으로 변경
# 변경 후 levels순서도 변경
main.ds$Reached.on.Time_Y.N <- relevel(main.ds$Reached.on.Time_Y.N, ref = "1")
head(main.ds$Reached.on.Time_Y.N)
levels(main.ds$Reached.on.Time_Y.N)

# 결측값 없음
colSums(is.na(main.ds))


# 1~ 8009의 데이터는 train데이터
# 나머지는 test데이터

tot <- nrow(main.ds)
train <- main.ds[c(1:8009), ]
test <- main.ds[c(8010 : tot), ]
test <- test %>% subset(select=-c(Reached.on.Time_Y.N))


scales::percent(nrow(train) / nrow(main.ds))
scales::percent(nrow(test) / nrow(main.ds))
# 데이터 비율 확인 


# 예측한 확률을 기록한 csv를 생성하시오.

parts <- createDataPartition(
    train$Reached.on.Time_Y.N, 
    p = 0.7,
    list = FALSE
)

t.train <- train[parts, ]
t.valid <- train[-parts, ]


model <- preProcess(
    t.train[, -12 ],
    method = c('range')
)

scaled_train <- predict(
    model, 
    t.train
)

scaled_valid <- predict(
    model,
    t.valid
)

# 예측

rf <- randomForest(
    Reached.on.Time_Y.N ~ . -X.U.FEFF.ID ,
    t.train,
    ntree = 500,
    do.trace = T
)

rf.pred <- predict(
    rf,
    newdata = t.valid,
    type = 'response'
)

confusionMatrix(
    data = rf.pred,
    refer = t.valid$Reached.on.Time_Y.N
)

mean(rf.pred == t.valid$Reached.on.Time_Y.N)
# Accuracy : 0.7148

# SVM

svm.md <- svm(
    Reached.on.Time_Y.N ~ . -X.U.FEFF.ID ,
    t.train,
)

svm.pred <- predict(
    object = svm.md,
    newdata = t.valid
)

confusionMatrix(
    svm.pred,
    t.valid$Reached.on.Time_Y.N
)

mean(svm.pred == t.valid$Reached.on.Time_Y.N)
# Accuracy : 0.7082


# ========================================================

# 랜덤포레스트 선택

final_model <- randomForest(
    Reached.on.Time_Y.N ~ . -X.U.FEFF.ID ,
    train,
    do.trace = T,
    ntree = 500
)

final_pred <- predict(
    final_model,
    newdata = test,
    type = 'prob'
)

# confusionMatrix(
#     final_pred,
#     test$Reached.on.Time_Y.N
# )

head(final_pred)
result_list <- final_pred[,1]
head(result_list)

result <- data.frame(
    test$X.U.FEFF.ID,
    result_list
)


names(result) <- c("ID", "pred")
head(result)

write.csv(result, "0603result.csv", row.names = F)

result_test <- read.csv("0603result.csv")
head(result_test)
