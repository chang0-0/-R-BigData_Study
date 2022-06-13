main.ds <- read.csv(
        "https://raw.githubusercontent.com/Datamanim/datarepo/main/bank/train.csv", 
        na.strings = c("na", "", "NA"), 
        stringsAsFactor = TRUE,
        header = TRUE)                                                                                              
head(main.ds)

# 마케팅 응답 고객들의 나이를 10살 단위로 변환 했을 때, 
# 가장 많은 인원을 가진 나이대는?

ds1 <- main.ds
head(ds1)
str(ds1)
ds1$age <- as.numeric(ds1$age)
ds1 <- ds1[order(ds1$age), ]
# ds1의 age컬럼을 범주형으로 전환

ds1$age[ ds1$age < 20 ] = 10
ds1$age[ ds1$age >= 20 & ds1$age < 30 ] = 20
ds1$age[ ds1$age >= 30 & ds1$age < 40 ] = 30
ds1$age[ ds1$age >= 40 & ds1$age < 50 ] = 40
ds1$age[ ds1$age >= 50 & ds1$age < 60 ] = 50
ds1$age[ ds1$age >= 60 & ds1$age < 70 ] = 60
ds1$age[ ds1$age >= 70 & ds1$age < 80 ] = 70
ds1$age[ ds1$age >= 80 & ds1$age < 90 ] = 80
ds1$age[ ds1$age >= 90 & ds1$age < 100 ] = 90


head(ds1$age, 100)
tail(ds1$age, 100)
ds1$age <- as.factor(as.character(ds1$age))
str(ds1)

# 가장 많은 인원을 가진 나이대는?
tb <- table(ds1$age)
tb <- sort(tb, decreasing = TRUE)
result1 <- tb[1]
result1 <- names(result1)
print(result1)


# 2 번
# 마케팅 응답 고객들의 나이를 10살 단위로 변경했을 때,가장 많은 나이대 구간의 인원은?
result2 <- tb1[[1]]
print(result2)

# 3번
# Q3. 나이가 25살 이상 29살 미만인 응답 고객들중 housing컬럼의 값이 yes인 고객의 수는?

ds2 <- main.ds
head(ds2)

library(dplyr)
temp <- ds2 %>% filter(age >= 25 & age < 29) 
temp <- temp %>% filter(housing == "yes")
result3 <- nrow(temp)
print(result3)

# 4번
# Q4. numeric한 값을 가지지 않은 컬럼들중 unique한 값을 가장 많이 가지는 컬럼은?

ds4 <- main.ds
str(ds4)

temp <- ds4 %>% select_if(is.factor)
str(temp)

n <- ncol(temp)
# 각 컬럼의 levels의 값들을 저장

temp2 <- sapply(
    temp,
    FUN = function(x) {
        length(levels(x))
    }
)

temp2 <- sort(temp2, decreasing = TRUE)
temp2

result4 <- temp2[1]
print(result4)

# 5번
# Q5. balance 컬럼값들의 평균값 이상을 가지는 데이터를
# ID값을 기준으로 내림차순 정렬했을때 상위 100개 데이터의 balance값의 평균은?

ds5 <- main.ds
head(ds5)
str(ds5)

balance.mean <- mean(ds5$balance)
balance.mean 

ds5 <- ds5 %>% filter( balance >= balance.mean )

ds5 <- ds5[ order( -ds5$ID) ,]
ds5.100 <- ds5[1:100, ]
ds5.100

result5 <- mean(ds5.100$balance)
print(result5)


# 6번
# Q6. 가장 많은 광고를 집행했던 날짜는 언제인가? (데이터 그대로 일(숫자),달(영문)으로 표기)

ds6 <- main.ds
str(ds6)
head(ds6, 10)

library(reshape2)

tb6 <- table(ds6$month)
tb6 <- which.max(tb6)
month.max <- names(tb6)

ds6 <- ds6 %>% filter( month == month.max )

tb6 <- table(ds6$day)
tb6 <- sort(tb6, decreasing = TRUE)
day.max <- names(tb6[1] )
day.max <- as.numeric(day.max)
day.max

result6 <- data.frame(day.max, month.max)
result6

# 7번
#  데이터의 job이 unknown 상태인 고객들의 age 컬럼 값의 정규성을 검정하고자 한다. 샤피로 검정의 p-value값을 구하여라

ds7 <- main.ds
ds7.unknown <- ds7 %>% filter( job == "unknown")
ds7.sh <- shapiro.test(ds7.unknown$age)
result7 <- ds7.sh$p.value
print(result7)


# 8번
# age와 balance의 상관계수를 구하여라

ds8 <- main.ds
result8 <- cor(ds8$age, ds8$balance)
print(result8)


# 9번
# y 변수와 education 변수는 독립인지 카이제곱검정을 통해 확인하려한다. p-value값을 출력하라

ds9 <- main.ds
ch <- chisq.test(ds9$y, ds9$education)
result9 <- ch$p.value
print(result9)


# 10번
# Q10. 각 job에 따라 marital컬럼의 divorced/married 인원의 비율을 확인 했을 때 그 값이 가장 높은 값은?

ds10 <- main.ds
str(ds10)
head(ds10 ,10)

ds10 <- ds10[, names(ds10)%in% c("job", "marital") ]
head(ds10, 100)

library(dplyr)
test <- ds10 %>% group_by(job) %>% summarize( ra = n() )
test


