library(ISLR)
library(dplyr)

data(Carseats)
main.ds <- Carseats
head(main.ds)

# 매출 (Sales)의 이상값을 제외한 데이터를 훈련 데이터로 선정할 때,
# Age의 표준편차를 구하시오 ( 이상값은 평균보다 1.5 표준편차 미만이거나, 초과인 값으로 한다.)
# 평균 - 1.5표준편차 

sale.temp <- main.ds$Sales
sale.temp

mean.temp <- mean(sale.temp)
sd.temp <- sd(sale.temp) * 1.5

over.outlier <- mean.temp + sd.temp
down.outlier <- mean.temp - sd.temp

result1 <- main.ds %>% filter(Sales <= over.outlier & Sales >= down.outlier ) %>% summarise(sd = sd(Age))
print(result1)


# 2번
library(MASS)
data(Cars93)
main.ds <- Cars93
head(main.ds)

# 변환전, 
mean1 <- mean( main.ds$Luggage.room, na.rm = TRUE)
mean1

main.ds$Luggage.room[ is.na(main.ds$Luggage.room) ] <- median(main.ds$Luggage.room, na.rm = TRUE)

colSums(is.na(main.ds))
mean2 <- mean( main.ds$Luggage.room)
mean2

result2 <- abs(mean1 - mean2)
result2


# 3번
setwd("C:/Users/Samsung/Desktop/빅분기실기준비/최종모의고사2회")

main.ds <- read.csv(
    "TimeAge.csv",
    stringsAsFactor = TRUE,
    na.strings = c("na", "", "NA"),
    encoding = "UTF-8"
)


# age가 20대인 확진자의 평균과 50대인 확진자의 평균의 차이의 절대값

library(dplyr)
main.20 <- main.ds %>% filter( age == "20s" ) %>% summarise( m1 = mean(confirmed) )
main.20

main.50 <- main.ds %>% filter( age == "50s" ) %>% summarise( m1 = mean(confirmed) )
main.50

result3 <- abs(main.20 - main.50)
print(result3)


