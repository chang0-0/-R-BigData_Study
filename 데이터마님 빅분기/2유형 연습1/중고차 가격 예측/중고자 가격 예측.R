library(dplyr)
library(tidyr)
library(ModelMetrics)
library(readr)
library(caret)

# 데이터 설명 : 중고차 가격 예측 데이터 (종속변수 :price)

x_train <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/carsprice/X_train.csv',
    encoding = 'UTF-8',
    na.strings = c('', ' ', 'NA', NA),
    stringsAsFactor = TRUE
)

y_train <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/carsprice/y_train.csv',
    encoding = 'UTF-8',
    na.strings = c('', ' ', 'NA', NA),
    stringsAsFactor = TRUE
)

x_test <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/carsprice/X_test.csv',
    encoding = 'UTF-8',
    na.strings = c('', ' ', 'NA', NA),
    stringsAsFactor = TRUE
)

x_label <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/carsprice/y_test.csv',
    encoding = 'UTF-8',
    na.strings = c('', ' ', 'NA', NA),
    stringsAsFactor = TRUE
)

cor.test(full$price, full$tax, method = 'pearson')
cor.test(full$price, full$tax, method = 'kendall')
cor.test(full$price, full$tax, method = 'spearman')

library(psych)
str(iris)

corr.test(
    y = iris$Sepal.Length,
    x = iris$Sepal.Width,
    use = 'complete',
    method = 'pearson',
    adjust = 'none'
)



full <- merge(x_train, y_train, by = 'carID')
full <- full[, -c(1)]

colSums(is.na(full))
# 결측값은 없음

# full$transmission의 Ohter 컬럼은 제거
full <- full %>% filter(  transmission %in% c('Automatic', 'Manual', 'Semi-Auto')  )
x_test <- x_test %>% filter(  transmission %in% c('Automatic', 'Manual', 'Semi-Auto')  )


str(full)
temp <- full %>% filter(brand == 'bmw')
temp



# 예측하기
model.glm <- lm(price ~ ., data = full)
summary(model.glm)

model.glm2 <- step(
    model.glm,
    direction = 'both'
)

summary(model.glm2)
glm.pred <- predict(
    model.glm2,
    newdata = x_test
)

glm.pred <- ifelse(
    glm.pred < 0, min(x_label$price), glm.pred
)

glm.pred <- round(glm.pred)
rmse(glm.pred, x_label$price)
