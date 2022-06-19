library(dplyr)

main <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/insurance/train.csv',
    encoding = 'UTF-8'
)

# Q1. Vehicle_Age 값이 2년 이상인 사람들만 필터링 하고 그중에서
# Annual_Premium 값이 전체 데이터의 중간값 이상인 사람들을 찾고, 그들의 Vintage값의 평균을 구하여라

ds1 <- main %>% filter(Vehicle_Age == '> 2 Years')
me <- median(main$Annual_Premium)
result1 <- ds1 %>% filter(Annual_Premium >= me) %>% summarise(mean = mean(Vintage))
print(result1)


# 2. Vehicle_age에 따른 각 성별(gender)그룹의 
# Annual_Premium값의 평균을 구하여 아래 테이블과 동일하게 구현하라

ds2 <- main
temp <- aggregate(
    Annual_Premium ~ Vehicle_Age + Gender,
    ds2,
    mean
)

library(reshape2)
result2 <- dcast(temp, Vehicle_Age ~ Gender, value.var = 'Annual_Premium' )
result2
