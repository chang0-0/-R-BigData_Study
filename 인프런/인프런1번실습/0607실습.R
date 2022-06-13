library(dplyr)
library(lubridate)
library(reshape2)

#### [[ 작업형 제 1유형 ]] ####
# Q1. 평일 기준으로 오후 1시 casual의 평균대비 registered의 평균이 얼마나 많은지
# 비율을 산출하고 반올림하여 소수점 둘째 자리까지 출력하시오.
# 답안 예시: 1.23

# Q2. 데이터 수집일 중 맑은 날의 비중을 백분률로 소수점 둘째 자리까지 출력하시오.
# ※ 맑은 날은 weather가 1인 비중이 가장 많은 날을 기준으로 한다.
# 답안 예시: 12.34

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/인프런/인프런1번실습")
list.files()

main <- read.csv(
    file = "data1.csv",
    fileEncoding = 'UTF-8-BOM',
)

str(main)
head(main, 10)
summary(main)

# 오후 1시 casual의 평균 대비 registred의 평균이 얼마나 많은지

main$hour <- hour(main$datetime)
head(main, 2)


# 시간 별 registerd와 hour의 평균
temp <- aggregate(
    data = main[, c("casual", "registered", "hour")],
    . ~ hour, 
    FUN = "mean"
)

temp2 <- temp[temp$hour == 13, ]
result1 <- round(temp2$registered / temp2$casual, 2)
print(result1)


# 2번 데이터 수집일 중 맑은 날의 비중을 백분률로 소수점 둘째 자리까지 출력하시오.
# ※ 맑은 날은 weather가 1인 비중이 가장 많은 날을 기준으로 한다.
# 답안 예시: 12.34

# weather가 1인 비중이 가장 많은 날

main <- read.csv(
    file = "data2.csv",
    fileEncoding = 'UTF-8-BOM',
)

str(main)
head(main, 10)

# 날짜별로 날씨별 계산
head(temp, 40)
tail(temp, 40)

temp <- aggregate(
    data = cbind(main, count = 1),
    count ~ date + weather, 
    FUN = 'sum'
)

# 날짜별로 정렬
temp <- temp[order(temp$date, temp$weather), ]
head(temp)

# 날씨별로 각 날씨의 개수대로 분리
# fill은 없는 값은 0으로 채우라는 의미
ds_cast <- dcast(
    data = temp,
    date ~ weather, 
    value.var = 'count',
    fill = 0
)

head(ds_cast)
head(ds_cast[, -1])

# wwather에서 가장 높은 컬럼값 추출
ds_cast[, 'weather'] = apply(
    ds_cast[, c('1', '2', '3', '4')], 
    MARGIN = 1,
    FUN = 'which.max'
)

head(ds_cast)
prop.table(table(ds_cast$weather))
result2 <- round(prop.table(table(ds_cast$weather))[1] * 100, 2)
print(result2)