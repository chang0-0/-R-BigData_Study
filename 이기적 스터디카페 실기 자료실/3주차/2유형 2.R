library(dplyr)
library(caret)
library(randomForest)
library(ModelMetrics)
library(scales)

# 데이터 설명 : 핸드폰 가격예측 (price_range컬럼 0(저렴) ~3(매우비쌈) 범위 ) 

train <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/mobile/train.csv',
    encoding = 'UTF-8',
    na.strings = c('', ' ', NA)
)

test <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/mobile/test.csv',
    encoding = 'UTF-8',
    na.strings = c('', ' ', NA)
)

normal = function(x) {
    (x - min(x)) / (max(x) - min(x))
}


train$battery_power <- normal(train$battery_power)
head(train$battery_power)


str(train)
unique(train$m_dep)


# 종속변수 
train$price_range <- as.factor(train$price_range)

# blue
train$blue <- as.factor(train$blue)
test$blue <- as.factor(test$blue)

# three_g
train$three_g <- as.factor(train$three_g)
test$three_g <- as.factor(test$three_g)

# touch_screen
train$touch_screen <- as.factor(train$touch_screen)
test$touch_screen <- as.factor(test$touch_screen)

# wifi
train$wifi <- as.factor(train$wifi)
test$wifi <- as.factor(test$wifi)

# n_cores
train$n_cores <- as.factor(train$n_cores)
test$n_cores <- as.factor(test$n_cores)

# four_g
train$four_g <- as.factor(train$four_g)
test$four_g <- as.factor(test$four_g)

# dual_sim
train$dual_sim <- as.factor(train$dual_sim)
test$dual_sim <- as.factor(test$dual_sim)


set.seed(12)
parts <- sample(
    1:nrow(train),
    size = 0.7
)

t.train <- train[parts, ]
t.valid <- train[-parts, ]


model <- preProcess(
    t.train,
    method = c('range')
)

sd.train <- predict(
    model,
    t.train
)

summary(sd.train)


model <- preProcess(
    t.valid,
    method = c('range')
)

sd.valid <- predict(
    model,
    t.valid
)






# 데이터 스케일링

model <- preProcess(
    train,
    method = c('range')
)

train2 <- predict(
    model,
    train
)
summary(train2)
head(train2)

model <- preProcess(
    test[,1],
    method = c('range')
)

test2 <- predict(
    model,
    test
)
summary(test2)


rf <- randomForest(
    price_range ~ .,
    train2,
    do.trace = TRUE,
    ntree = 400
)


rf.p <- predict(
    rf,
    newdata = test
)

library(rpart)
rp <- rpart(
    price_range ~ .,
    train
)

rp.p <- predict(
    rp,
    newdata = test,
    type = 'class'
)

length(rp.p)

set.seed(21)
parts <- sample(
    1:nrow(train),
    size = 1000
)

sam <- train[parts, ]
s

caret::confusionMatrix(
    rp.p,
    sam$price_range
)

result <- data.frame(
    test$id, 
    rp.p
)

names(result) <- c('id', 'price_range')
head(result, 100)

