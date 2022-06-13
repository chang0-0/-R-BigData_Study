library(lubridate)
library(dplyr)


setwd("C:/Users/Samsung/Desktop/빅분기실기준비/인프런1번실습")
list.files()

ds1 <- read.csv("set_01_data_01.csv")
ds2 <- read.csv("set_01_data_02.csv")
head(ds1)
head(ds2)


#### [[ 작업형 제 1유형 ]] ####
# Q1. 평일 기준으로 오후 1시 casual의 평균대비 registered의 평균이 얼마나 많은지
# 비율을 산출하고 반올림하여 소수점 둘째 자리까지 출력하시오.
# 답안 예시: 1.23

# Q2. 데이터 수집일 중 맑은 날의 비중을 백분률로 소수점 둘째 자리까지 출력하시오.
# ※ 맑은 날은 weather가 1인 비중이 가장 많은 날을 기준으로 한다.
# 답안 예시: 12.34


# 평일 기준 오후 1시 casual의 평균
ds1$hour <- hour(ds1$datetime) 
head(ds1)

# 각 시간에 따른 casual, registered, hour의 데이터를 써서 평균값을 계산
ds_agg <- aggregate(data = ds1[, c("casual", "registered", "hour")], . ~ hour, FUN = "mean")

after_noon <- ds_agg %>% filter(hour == 13)
after_noon <- ds_agg[ds_agg$hour == 13, ]

casual <- after_noon$casual
regi <- after_noon$registered
result1 <- round(regi / casual, 2)
print(result1)


## 2번 ################################

# Q2. 데이터 수집일 중 맑은 날의 비중을 백분률로 소수점 둘째 자리까지 출력하시오.
# ※ 맑은 날은 weather가 1인 비중이 가장 많은 날을 기준으로 한다.
# 답안 예시: 12.34


head(ds2)
ds2_agg <- aggregate(data = cbind(ds2, count = 1), count ~ date + weather, FUN = "sum"  )

ds2_agg <- ds2_agg[order(ds2_agg$date, ds2_agg$weather ), ]
head(ds2_agg)

ds_02_case = dcast(data = ds2_agg)

