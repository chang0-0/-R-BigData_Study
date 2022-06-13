# 주어진 데이터의 첫 번째 행부터 순서대로 80%까지의 데이터를 훈련 데이터로 추출 후
# 'total_bedrooms' 변수의 결측값을 중앙값으로 대체하고
# 대체전의 'total_bedrooms' 변수 표준편차 값과 대체 후의 'total_bedrooms' 변수 표준편차 값의  차이의 
# 절대값을 출력하시오

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/220520")
ds <- read.csv("housing.csv")
head(ds, 10)


# 첫번째 행부터 순서대로 80%의 데이터를 훈련데이터로 변환
n <- nrow(ds) * 0.8
train <- ds[ c(1:n), ]
nrow(train)
head(train, 10)

# 결측값 대체전의 중앙값
str(train)
mid <- median(train$total_bedrooms, na.rm=TRUE)
mid


# 결측값 대체전의 표준편차
before_sd <- sd(train$total_bedrooms, na.rm = TRUE)
before_sd

train$total_bedrooms[is.na(train$total_bedrooms)] <- mid
head(train$total_bedrooms, 100)


# 결측값 대체후의 표준편차
after_sd <- sd(train$total_bedrooms)
result <- abs(before_sd - after_sd)
print(result)

