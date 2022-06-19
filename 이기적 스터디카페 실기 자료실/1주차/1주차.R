library(dplyr)
library(lubridate)

main <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/weather/weather2.csv',
    fileEncoding = 'UTF-8-BOM'
)


# Q1. 여름철(6월,7월,8월) 이화동이 수영동보다 높은 기온을 가진 시간대는 몇개인가?

ds1 <- main
temp <- ds1 %>% filter( month(time) == c('6', '7', '8') & 이화동기온 > 수영동기온)
result1 <- nrow(temp)
print(result1)


# Q2. 이화동과 수영동의 최대강수량의 시간대를 각각 구하여라

ds2 <- main
temp <- ds2 %>% filter(max(이화동강수) == 이화동강수 | max(수영동강수) == 수영동강수)
temp <- temp[order(-temp$이화동강수), ]
result2 <- temp$time
print(result2)
