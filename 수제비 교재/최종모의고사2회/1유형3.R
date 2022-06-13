library(dplyr)


setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/최종모의고사2회")
main <- read.csv(
    file = "TimeAge.csv",
    fileEncoding = "UTF-8-BOM"
)

str(main)

# age가 20대인 confirmed의 평균과 50인 확진자 평균 차이의 절댓값을 구하시오


main20 <- main %>% filter(age == '20s') %>% summarise(mean = mean(confirmed))
main50 <- main %>% filter(age == '50s') %>% summarise(mean = mean(confirmed))

result3 <- abs(main20 - main50)
print(result3)
