library(dplyr)
library(caret)
library(ggplot2)
library(ModelMetrics)
library(readr)

# 데이터 설명 : 학생성적 예측 (종속변수 :G3)

x_train <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/studentscore/X_train.csv',
    encoding = 'UTF-8',
    na.strings = c('', ' ', 'NA', NA),
    stringsAsFactor = TRUE
)

y_train <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/studentscore/y_train.csv',
    encoding = 'UTF-8',
    na.strings = c('', ' ', 'NA', NA),
    stringsAsFactor = TRUE
)

x_test <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/studentscore/X_test.csv',
    encoding = 'UTF-8',
    na.strings = c('', ' ', 'NA', NA),
    stringsAsFactor = TRUE
)

x_label <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/studentscore/y_test.csv',
    encoding = 'UTF-8',
    na.strings = c('', ' ', 'NA', NA),
    stringsAsFactor = TRUE
)


full <- merge(x_train, y_train, by = 'StudentID')

# StudentID 컬럼은 예측에서 필요없으므로 제거
full <- full[, -c(1)]

# 결측값 확인
colSums(is.na(full))
colSums(is.na(x_test))

# 결측값은 하나도 없음


# 데이터 전처리
unique(full$famrel)
str(full)

full$freetime <- as.factor(full$freetime)
x_test$freetime <- as.factor(x_test$freetime)

full$Medu <- as.factor(full$Medu)
x_test$Medu <- as.factor(x_test$Medu)

full$Fedu <- as.factor(full$Fedu)
x_test$Fedu <- as.factor(x_test$Fedu)

full$studytime <- as.factor(full$studytime)
x_test$studytime <- as.factor(x_test$studytime)

full$traveltime <- as.factor(full$traveltime)
x_test$traveltime <- as.factor(x_test$traveltime)

full$failures <- as.factor(full$failures)
x_test$failures <- as.factor(x_test$failures)

full$goout <- as.factor(full$goout)
x_test$goout <- as.factor(x_test$goout)

full$health <- as.factor(full$health)
x_test$health <- as.factor(x_test$health)

full$Walc <- as.factor(full$Walc)
x_test$Walc <- as.factor(x_test$Walc)

full$Dalc <- as.factor(full$Dalc)
x_test$Dalc <- as.factor(x_test$Dalc)

full$famrel <- as.factor(full$famrel)
x_test$famrel <- as.factor(x_test$famrel)

summary(full)


model <- preProcess(
    full[, -c(31, 32, 33)],
    method = c('range')
)

full <- predict(
    model,
    full
)

# 예측 전 작업
train.model <- lm(G3 ~ ., data = full )
summary(train.model)

train.model2 <- step(train.model, direction = 'both')
summary(train.model2)


# 수제비교재를 보고 예측
set.seed(2101)
model_glm <- train(x = full[, -c(33)], y = full[, 33], 
    method = 'glm',
    trControl = trainControl(method = 'cv', number = 5),
    metric = 'RMSE',
    verbose = FALSE
)

model_glm
getTrainPerf(model_glm)

glm.pred <- predict(
    model_glm,
    newdata = x_test
)

head(glm.pred)

rmse(glm.pred , x_label$G3) 
# 13.47771

min(glm.pred)
max(glm.pred)




##################################################################


pred <- predict(
    train.model2,
    newdata = x_test
)

pred <- ifelse(pred < 0, 0, round(pred))
head(pred)

rmse(pred, x_label$G3)
# 11.47611

result <- data.frame(
    x_test$StudentID,
    pred
)


names(result) <- c('StudentID', 'G3')
head(result)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/데이터마님 빅분기/2유형 연습1/학생성적 예측")
write.csv(result, "result.csv", row.names = FALSE)
