library(dplyr)
library(caret)
library(rpart)
library(ModelMetrics)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/빅분기 체험")
list.files()

X_test <- read.csv(
    file = "X_test.csv",
    encoding = 'UTF-8-BOM',
    stringsAsFactor = TRUE
)

X_train <- read.csv(
    file = "X_train.csv",
    encoding = 'UTF-8-BOM',
    stringsAsFactor = TRUE
)

y_train <- read.csv(
    file = "y_train.csv",
    encoding = 'UTF-8-BOM',
    stringsAsFactor = TRUE
)

full <- merge(
    X_train,
    y_train,
    by = 'cust_id'
)


# 결측치 처리 및 이상치 변환
full <- full %>% filter( !cust_id %in% c(1521, 2035))
full$주구매지점 <- as.factor(full$주구매지점)
full$주구매상품 <- as.factor(full$주구매상품)
full[is.na(full$환불금액), ]$환불금액 <- 0
full$gender <- as.factor(full$gender)


X_test$주구매지점 <- as.factor(X_test$주구매지점)
X_test$주구매상품 <- as.factor(X_test$주구매상품)
X_test[is.na(X_test$환불금액), ]$환불금액 <- 0

full <- full[, -1]



train_up <- upSample(full[,-10], full[,10], list = F, yname = 'gender') 
dim(train_up)

tr_x <- train_up[,c(1:9)]
tr_y <- train_up[,c(10)] 
tr_y <- as.data.frame(
    tr_y
)
names(tr_y) <- 'gender'

model <- preProcess(
    tr_x,
    method = c('range')
)

tr_x <- predict(
    model,
    tr_x
)

tr <- cbind(tr_x, tr_y)

rf <- randomForest(
    gender ~ .,
    tr,
    ntree = 300,
    do.trace = TRUE
)

pred <- predict(
    rf, 
    neadata = X_test,
    type = 'prob'
)

auc(rf)

# 남자일 확률 남자 -> 1
head(pred)
list <- pred[,1]
length(list)
list <- list[1:nrow(X_test)]

result <- data.frame(
    X_test$cust_id, 
    list
)

names(result) <- c('cust_id', 'gender')
write.csv(result, "000.csv", row.names = FALSE)
test.result <- read.csv(
    '000.csv'
)

head(test.result)

nrow(test.result)
auc(test.result)
auc(rf)
