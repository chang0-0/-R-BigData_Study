library(dplyr)

main.ds <- read.csv(
    file = "https://raw.githubusercontent.com/Datamanim/datarepo/main/happy2/happiness.csv",
    encoding = 'UTF-8',
    stringsAsFactor = TRUE  
)

str(main.ds)
summary(main.ds)

# 1번 데이터는 2018년도와 2019년도의 전세계 행복 지수를 표현한다. 
# 각년도의 행복랭킹 10위를 차지한 나라의 행복점수의 평균을 구하여라

# 년도별 행복랭킹

ds1 <- main.ds
ds1$count <- 1
summary(ds1)

temp1 <- aggregate(
    점수 ~ 년도 + 행복랭킹 + 나라명,
    ds1,
    FUN = sum
)


temp1 <- temp1 %>% filter(행복랭킹 == 10)
result1 <- mean(temp1$점수)
print(result1)


# 2번 데이터는 2018년도와 2019년도의 전세계 행복 지수를 표현한다. 
# 각년도의 행복랭킹 50위이내의 나라들의 각각의 행복점수 평균을 데이터프레임으로 표시하라

library(dplyr)
ds2 <- main.ds

agg <- aggregate(
    행복랭킹 ~ 년도 + 점수 + 나라명,
    ds2,
    sum
)

agg <- agg %>% filter(행복랭킹 <= 50)
agg <- agg[order(agg$행복랭킹), ]

temp <- agg %>% group_by(년도) %>% summarise(mean = mean(점수))
result2 <- temp
print(result2)


# 3번 2018년도 데이터들만 추출하여 행복점수와 부패에 대한 인식에 대한 상관계수를 구하여라

ds3 <- main.ds
ds3 <- ds3 %>% filter(년도 == 2018)
summary(ds3)
result3 <- cor.test(ds3$점수, ds3$부패에.대한인식)
print(result3$estimate)


# 4번 2018년도와 2019년도의 행복랭킹이 변화하지 않은 나라명의 수를 구하여라

ds4 <- main.ds
summary(ds4)

temp1 <- ds4 %>% filter(년도 == 2018) %>% select(c(년도, 나라명, 행복랭킹))
temp2 <- ds4 %>% filter(년도 == 2019) %>% select(c(년도, 나라명, 행복랭킹))


# 5번 2019년도 데이터들만 추출하여 각변수간 상관계수를 구하고 내림차순으로 
# 정렬한 후 상위 5개를 데이터 프레임으로 출력하라
# 컬럼명은 v1,v2,corr으로 표시하라

ds5 <- main.ds
summary(ds5)

ds5 <- ds5 %>% filter(년도 == 2019) %>% select( -c(년도, 나라명) )
str(ds5)

result5 <- as.data.frame(as.table(cor(ds5)))
names(result5) <- c('v1', 'v2', 'corr')
result5


# 6번 각 년도별 하위 행복점수의 하위 5개 국가의 평균 행복점수를 구하여라

ds6 <- main.ds
