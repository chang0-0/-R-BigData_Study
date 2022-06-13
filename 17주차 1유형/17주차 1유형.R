setwd("C:/Users/Samsung/Desktop/빅분기실기준비/17주차 1유형")
main.ds <- read.csv( 'https://raw.githubusercontent.com/Datamanim/datarepo/main/body/body.csv',
   encoding = "UTF-8"
 )
library(dplyr)
head(main.ds)

# 1번 전체데이터의 수축기혈압(최고) - 이완기혈압(최저)의 평균을 구하여라

ds1 <- main.ds
str(ds1)
ds1$평균 <- NA
ds1$평균 <- ds1$수축기혈압.최고....mmHg - ds1$이완기혈압.최저....mmHg
result1 <-  mean(ds1$평균)
print(result1)

# 2번 50~59세의 신장평균을 구하여라

ds2 <- main.ds
library(dplyr)
ds2.temp <- ds2 %>% filter(측정나이 >= 50 & 측정나이 < 60) %>% summarise(mean = mean(신장...cm))
print(ds2.temp)


# 3번 연령대 (20~29 : 20대 ...) 별  인원수를 구하여라

ds3 <- main.ds
min(ds3$측정나이)
max(ds3$측정나이)
ds3$측정나이[ds3$측정나이 >= 20 & ds3$측정나이 <= 29] <- 20
ds3$측정나이[ds3$측정나이 >= 30 & ds3$측정나이 <= 39] <- 30
ds3$측정나이[ds3$측정나이 >= 40 & ds3$측정나이 <= 49] <- 40
ds3$측정나이[ds3$측정나이 >= 50 & ds3$측정나이 <= 59] <- 50
ds3$측정나이[ds3$측정나이 >= 60 & ds3$측정나이 <= 69] <- 60
ds3$측정나이 <- as.factor(ds3$측정나이)

ds3.temp <- cbind(ds3, count = 1)
result3 <- aggregate(
    data = ds3.temp,
    count ~ 측정나이,
    FUN = sum
)

names(result3) <- c("연령대", "인원수")
print(result3)


# 4번 연령대 (20~29 : 20대 ...) 별 등급의 숫자를 데이터 프레임으로 표현하라

ds4 <- main.ds
ds4$측정나이[ds4$측정나이 >= 20 & ds4$측정나이 <= 29] <- 20
ds4$측정나이[ds4$측정나이 >= 30 & ds4$측정나이 <= 39] <- 30
ds4$측정나이[ds4$측정나이 >= 40 & ds4$측정나이 <= 49] <- 40
ds4$측정나이[ds4$측정나이 >= 50 & ds4$측정나이 <= 59] <- 50
ds4$측정나이[ds4$측정나이 >= 60 & ds4$측정나이 <= 69] <- 60
ds4$측정나이[ds4$측정나이 >= 70 & ds4$측정나이 <= 79] <- 70
ds4$측정나이 <- as.factor(ds4$측정나이)

ds4.temp <- cbind(ds4, count = 1)
head(ds4.temp)

result4 <- aggregate(
    data = ds4.temp,
    count~ 측정나이 + 등급,
    FUN = sum
)

result4 <- result4[order(result4$측정나이), ]
rownames(result4) <- NULL
print(result4)


# 5번 남성 중 A등급과 D등급의 체지방률 평균의 차이(큰 값에서 작은 값의 차)를 구하여라
ds5 <- main.ds
ds5.temp <- ds5 %>% filter(측정회원성별 == "M" & (등급 == 'A' | 등급 == 'D'))
A.체지방율 <- ds5.temp %>% filter(등급 == 'A') %>% summarise(Amean = mean(체지방율....))
D.체지방율 <- ds5.temp %>% filter(등급 == 'D') %>% summarise(Dmean = mean(체지방율....))

result5 <- abs(A.체지방율 - D.체지방율)
print(result5)


# 6번 여성 중 A등급과 D등급의 체중의 평균의 차이(큰 값에서 작은 값의 차)를 구하여라

ds6 <- main.ds
ds6.temp <- ds6 %>% filter(측정회원성별 == "F" & (등급 == 'A' | 등급 == 'D'))
A.체지방율 <- ds6.temp %>% filter(등급 == 'A') %>% summarise(Amean = mean(체중...kg))
D.체지방율 <- ds6.temp %>% filter(등급 == 'D') %>% summarise(Dmean = mean(체중...kg))

result6 <- abs(A.체지방율 - D.체지방율)
print(result6)


# 7번. bmi는 자신의 몸무게(kg)를 키의 제곱(m)으로 나눈값이다. 
# 데이터의 bmi 를 구한 새로운 컬럼을 만들고 남성의 bmi 평균을 구하여라


ds7 <- main.ds
ds7$bmi <- NA
ds7$bmi <-  (ds7$체중...kg / (ds7$신장...cm/100)^2   )
ds7 <- ds7 %>% filter(측정회원성별 == 'M')
result7 <- mean(ds7$bmi)
print(result7)


# 8번. bmi보다 체지방율이 높은 사람들의 체중평균을 구하여라

ds8 <- main.ds
ds8$bmi <- NA
ds8$bmi <- ( ds8$체중...kg / (ds8$신장...cm/100)^2 )

result8 <- ds8 %>% filter(bmi < 체지방율....) %>% summarise(mean = mean(체중...kg))
print(result8)


# 9번. 남성과 여성의 악력 평균의 차이를 구하여라

ds9 <- main.ds
ds9.m <- ds9 %>% filter( 측정회원성별 == 'M' ) %>% summarise(man.mean = mean(악력D...kg))
ds9.f <- ds9 %>% filter( 측정회원성별 == 'F' ) %>% summarise(woman.mean = mean(악력D...kg))
result9 <- abs(ds9.m - ds9.f)
print(result9)


# 10번 남성과 여성의 교차윗몸일으키기 횟수의 평균의 차이를 

ds10 <- main.ds
ds10.m <- ds10 %>% filter( 측정회원성별 == 'M' ) %>% summarise(man.mean = mean(교차윗몸일으키기...회))
ds10.w <- ds10 %>% filter( 측정회원성별 == 'F' ) %>% summarise(woman.mean = mean(교차윗몸일으키기...회))
result10 <- abs(ds10.m - ds10.w)
print(result10)
