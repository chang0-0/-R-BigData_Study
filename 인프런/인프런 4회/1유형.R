library(dplyr)

# 1. 학력 수준과 거주 도시의 개발 지수의 상관관계를 알아보고자 한다.
# city_dev_level과 edu_level을 대상으로 Spearman 순위상관분석을 실시하고 
# 그 상관계수를 반올림하여 소수점 셋째 자리까지 출력하시오.

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/인프런/인프런 4회")
list.files()

main <- read.csv(
    file = "set_04_data.csv",
    fileEncoding = 'UTF-8-BOM',
    stringsAsFactor = TRUE
)

temp <- main$edu_level
temp <- temp[1: 100]
temp

str(main)
temp <- factor(temp, ordered = TRUE)
temp


temp2 <- as.numeric(factor(
    temp, ordered = TRUE
))

temp2

cor.test(
    as.numeric(
        factor(main$edu_level, order = TRUE)
    ),
    main$city_dev_idx,
    method = 'spearman'
)
