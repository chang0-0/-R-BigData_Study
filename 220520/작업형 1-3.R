
setwd("C:/Users/Samsung/Desktop/빅분기실기준비/220520")
ds <- read.csv("insurance.csv", na.strings = c("NA", ""))

# 이상값의 합을 구하시오

# 이상값은 평균에서 1.5표준편차 이상인 값.

char <- ds$charges
mean <- mean(char)
mean

over <- sd(ds$charges) * 1.5
over

over_outlier <- mean + over
down_outlier <- mean - over

library(dplyr)
temp <- ds %>% filter(charges >= over_outlier | charges <= down_outlier)

result <- sum(temp$charges)
print(result)
