library(dplyr)
library(tidyr)
library(readr)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/기출3회")

main <- read.csv(
    file = 'TB_notifications_2022-06-19.csv',
    encoding = 'UTF-8',
    stringsAsFactor = TRUE
)


# 2000년의 국가의 평균 결핵 발생 건수를 구하고

main <- main %>% filter(year == 2000) %>% select(year, new_sp, country)
me <- mean(main$new_sp, na.rm = T)
main <- na.omit(main)
result3 <- main %>% filter(new_sp > me) %>% tally()
print(result3)
