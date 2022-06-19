library(dplyr)

main <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/smoke/train.csv',
    fileEncoding = 'UTF-8-BOM',
)

head(main)


# Q1. 시력(좌) 와 시력(우)의 값이 같은 남성의 허리둘레의 평균은?

ds1 <- main
result1 <- ds1 %>% filter(시력.좌. == 시력.우. & 성별코드 == 'M') %>% summarise(mean = mean(허리둘레))
print(result1)


# Q2. 40대(연령대코드 40,45) 여성 중 '총콜레스테롤'값이 40대 여성의 '총콜레스테롤' 
# 중간값 이상을 가지는 그룹과
# 50대(연령대코드 50,55) 여성 중 '총콜레스테롤'값이 50대 여성의 
#'총콜레스테롤' 중간값 이상을 가지는
 
#'두 그룹의 '수축기혈압'이 독립성,정규성,등분산성이 만족하는것을 확인했다. 
# 두 그룹의 '수축기혈압'의 독립표본 t 검증 결과를 통계값, p-value 구분지어 구하여라.

ds2 <- main
me40 <- ds2 %>% filter(연령대코드.5세단위. %in% c(40, 45) & 성별코드 == 'F')
me <- median(me40$총콜레스테롤)
me40 <- me40 %>% filter(총콜레스테롤 >= me)


me50 <- ds2 %>% filter(연령대코드.5세단위. %in% c(50, 55) & 성별코드 == 'F')
me <- median(me50$총콜레스테롤)
me50 <- me50 %>% filter(총콜레스테롤 >= me)

result <- t.test(me40$수축기혈압, me50$수축기혈압, var.equal = TRUE)
sta <- result$statistic
p <- result$p.value

result2 <- data.frame(
    sta, 
    p
)

print(result2)
