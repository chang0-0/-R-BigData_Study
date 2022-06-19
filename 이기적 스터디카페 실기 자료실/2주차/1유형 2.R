library(dplyr)
library(tidyr)

main <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/smoke/train.csv',
    encoding = 'UTF-8',
    sep = ','
)


# Q1. 수축기혈압과 이완기혈압 수치의 차이를 새로운 컬럼('혈압차') 으로 생성하고, 
# 연령대 코드별 각 그룹 중 '혈압차' 의 분산이 5번째로 큰 연령대 코드를 구하여라

ds1 <- main
ds1$혈압차 <- ds1$수축기혈압 - ds1$이완기혈압
temp <- ds1 %>% group_by(연령대코드.5세단위.) %>% summarise(var = var(혈압차))
temp <- temp[order(-temp$var ), ]
result1 <- temp$연령대코드.5세단위.[5]
print(result1)


# Q2. 비만도를 나타내는 지표인 WHtR는 허리둘레 / 키로 표현한다. 
# 일반적으로 0.58이상이면 비만으로 분류한다.
# 데이터중 WHtR 지표상 비만인 인원의 남/여 비율을 구하여라

ds2 <- main
ds2$WHtR <- ds2$허리둘레 / ds2$신장.5Cm단위.
ds2 <- ds2 %>% filter(ds2$WHtR >= 0.58)
man <- ds2 %>% filter(성별코드 == 'M') %>% summarise(n())
woman <- ds2 %>% filter(성별코드 == 'F') %>% summarise(n())
result2 <- man / woman
print(result2)
