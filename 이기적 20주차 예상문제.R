library(dplyr)
library(tidyr)

main.ds <- read.csv(
    "https://raw.githubusercontent.com/Datamanim/datarepo/main/stroke_/train.csv",
    encoding = 'UTF-8-BOM')

str(main.ds)
head(main.ds$age)


# 1.성별이 Male인 환자들의 age의 평균값은?
ds1 <- main.ds
ds1$age <- as.numeric(ds1$age)
ds1 <- ds1 %>% filter(gender == 'Male')
mean(ds1$age, na.rm = TRUE)


# 2. bmi컬럼의 결측치를 제외한 나머지 값들의 중앙값으로 채웠을 경우
# bmi 컬럼의 평균을 소숫점 이하 3자리 까지 구하여라
ds2 <- main.ds
me <- median(ds2$bmi, na.rm = T)

ds2$bmi[is.na(ds2$bmi)] <- me
result2 <- round(mean(ds2$bmi),3 )
print(result2)


# 3. bmi컬럼의 각 결측치들을 직전의 행의 bmi값으로 채웠을 경우 
# bmi 컬럼의 평균을 소숫점 이하 3자리 까지 구하여라

ds3 <- main.ds
colSums(is.na(ds3))
ds3$bmi
ds3 <- ds3 %>% fill(bmi)
result3 <- round(mean(ds3$bmi), 3)
print(result3)


# 4. bmi컬럼의 각 결측치들을 결측치를 가진 환자 나이대(10단위)의 평균 
# bmi 값으로 대체한 후 대체된 bmi 컬럼의 평균을 소숫점 이하 3자리 까지 구하여라

ds4 <- main.ds
ds4[ds4$age >= 1 & ds4$age <= 9] <- 1
temp <- ds4 %>% filter(is.na(bmi))
min(temp$age)
max(temp$age)


# 5. avg_glucose_level 컬럼의 값이 200이상인 데이터를 모두 199로 
# 변경하고 stroke값이 1인 데이터의 avg_glucose_level값의 평균을 소수점이하 3자리 까지 구하여라

ds5 <- main.ds
ds5$avg_glucose_level[ds5$avg_glucose_level >= 200] <- 199
ds5 <- ds5 %>% filter(stroke == 1) %>% summarise( mean = round(mean(avg_glucose_level), 3))
ds5


# 6. Attack컬럼의 값을 기준으로 내림차순정렬 했을때 상위 400위까지 포켓몬들과 
# 401~800위까지의 포켓몬들에서 전설포켓몬(Legendary컬럼)의 숫자 차이는?

main.ds <- read.csv(
    "https://raw.githubusercontent.com/Datamanim/datarepo/main/pok/Pokemon.csv",
    fileEncoding = 'UTF-8-BOM'
)

ds6 <- main.ds
ds6 <- ds6[order(-ds6$Attack), ]
ds6$Attack

top <- ds6[1:400, ]
bottom <- ds6[401:800, ]
top <- top %>% filter(Legendary == "True") %>% summarise(n())
bottom <- bottom %>% filter(Legendary == "True") %>% summarise(n())

result6 <- abs(top - bottom)df
print(result6)


# 7. Type 1 컬럼의 종류에 따른 Total 컬럼의 평균값을 내림차순 정렬했을때 상위 3번째 Type 1은 무엇인가?

ds7 <- main.ds
