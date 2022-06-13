library(dplyr)
library(caret)
library(scales)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/2022-05-24")
data(mtcars)
main.ds <- mtcars

# 7:3의 훈련데이터와 test데이터로 분리

set.seed(122)
parts <- sample(
    1:nrow(main.ds),
    size = nrow(main.ds)*.7
)

train <- main.ds[parts, ]
test <- main.ds[-parts, ]

scales::percent(nrow(train) / nrow(main.ds))

# mpg 연비를 예측하는 최적 모델을 만들어라

model_train <- lm(mpg ~ ., train)
step(model_train, direction = "both")

formul <- mpg ~ cyl + wt

summary( lm(formul, train) )

md_step <- lm(formul, train)
pred <- predict(
    md_step,
    newdata = test,
    type = "response"
)

pred
library(ModelMetrics)
rmse(test$mpg, pred)
