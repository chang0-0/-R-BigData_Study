library(dplyr)
library(MASS)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/단원종합문제 1유형")
list.files()

# 다음은 music 데이터 세트이다.
# tempo 항목의 상위 25%와 하위 25%를 0으로 대체하여 훈련 데이터를 생성하고
# tempo항목의 평균과 표준편차의 합을 구하시오
# (단 5분위수는 fivenum 함수를 사용한다.)

main <- read.csv(
    file = 'data.csv',
    fileEncoding = 'UTF-8-BOM',
    stringsAsFactor = TRUE
)

head(main, 10)
str(main)

fivenum(main$tempo)
quantile(main$tempo)
summary(main$tempo)

lower <- fivenum(main$tempo)[2]
lower

upper <- fivenum(main$tempo)[4]
upper

main$tempo[main$tempo <= lower | main$tempo >= upper] <- 0
result5 <- mean(main$tempo) + sd(main$tempo)
print(result5)

