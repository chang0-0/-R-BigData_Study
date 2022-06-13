library(dplyr)
library(caret)
library(ggplot2)
library(scales)

# diamonds의 데이터를 순서대로 80% 데이터를 훈련 데이터로 만들어서 price 기준으로 상위 100개의 데이터에 대한
# price의 평균을 구하시오

data(diamonds)

main <- diamonds

n <- nrow(main) * 0.8
train <- main[1:n, ]

train <- train[order(-train$price), ]
head(train, 10)

temp <- train$price[1:100]
result3 <- mean(temp)
print(result3)
