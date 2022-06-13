setwd("C:/Users/Samsung/Desktop/빅분기실기준비/220520")

ds <- read.csv("housing.csv", na.strings = c("", "na", "NA"), stringsAsFactor = TRUE)
head(ds)
str(ds)

# 결측값이 있는 행을 전부 제거

ds <- na.omit(ds)
nrow(ds)

# 순서대로 70%의 데이터를 훈련데이터로 생성

n <- nrow(ds) * 0.7
train <- ds[ c(1:n) ,  ]
nrow(train)

# 70%의 데이터인지 확인
scales::percent( nrow(train) / nrow(ds)  )

str(train)

result <- quantile(train$housing_median_age)[2]
print(result)


