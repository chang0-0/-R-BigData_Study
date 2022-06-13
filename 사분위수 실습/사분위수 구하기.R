library(dplyr)
library(caret)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/사분위수 실습")
list.files()

ds <- read.csv("housing.csv")
head(ds)

# 결측값 파악 및 결측값 제거
colSums(is.na(ds))
ds <- na.omit(ds)
str(ds)

# 데이터의 순서대로 상위 70%를 훈련데이터로 생성

parts <- caret::createDataPartition(ds$longitude, p=0.7, list = FALSE)
str(parts)

ds[Resample1, ]


n_cnt <- nrow(ds) * 0.7
train <- ds[1:n_cnt, ]

nrow(train)
summary(train)

result <- quantile(train$housing_median_age)[2]
print(result)
