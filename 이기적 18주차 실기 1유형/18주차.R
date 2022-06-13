
main.ds <- read.csv(
    file = "https://raw.githubusercontent.com/Datamanim/datarepo/main/nba/nba.csv",
    stringsAsFactor = TRUE,
    header = TRUE,
    encoding = "UTF-8-BOM",
    sep = ';'
)
head(main.ds)
colSums(is.na(main.ds))

# 1. Tm 컬럼은 각 팀의 이름을 의미한다. TOR팀의 평균나이를 소수 4째 자리까지 구하여라

ds1 <- main.ds
ds1.TOR <- ds1 %>% filter(Tm == "TOR") %>% summarise( avg = round(mean(Age, na.rm = TRUE) , 4))
resutl1 <- ds1.TOR
print(resutl1)


# 2. Pos 컬럼은 포지션을 의미한다. 전체 선수 중 최소나이대의 선수들을 필터하고 
# 그들 중 가장 많은 포지션은 무엇인지 확인하라

ds2 <- main.ds
ds2$Pos <- as.character(ds2$Pos)
# 오름차순으로 정렬
ds2 <- ds2[ order(ds2$Age) ,]
ds2.10 <- ds2 %>% filter( min(ds2$Age) == ds2$Age )
# 가장 자주나온 Pos확인
# 각 포지션 별 count

ds2.10 <- cbind(ds2.10, count = 1)

# 각 Pos별 갯수
ds2.agg <- aggregate(data = ds2.10, count ~ Pos, FUN = sum)
result2 <- ds2.agg[ ds2.agg$count == max(ds2.agg$count), ]
result2 <- result2$Pos
print(result2)


# 3. 선수들의 이름은 first_name+ 공백 + last_name으로 이루어져 있다. 
# 가장 많은 first_name은 무엇이며 몇 회 발생하는지 확인하라

library(stringr)

ds3 <- main.ds
temp <- ds3$Player %>% str_split(pattern=" ", simplify = TRUE)
head(temp)

ds3$first_name <- temp[,1] %>% as.factor()
ds3$last_name <- temp[,2] %>% as.factor()

tb <- table(ds3$first_name)
tb <- sort(tb, decreasing = TRUE)
result3 <- tb[1]
print( names(result3) )


# 4. PTS컬럼은 경기당 평균득점수이다. 각포지션별로 경기당 평균득점수의 평균을 구하여라
ds4 <- main.ds

ds4 <- aggregate(
    data = ds4,
    PTS ~ Pos,
    mean
)
result4 <- ds4
result4

print(result4)


# 5. G컬럼은 참여한 경기의 숫자이다. 
# 각 팀별로 가장 높은 경기참여 수를 가진 선수들의 경기 참여 숫자의 평균을 구하여라

# 각 팀별 G컬럼이 가장 높은 선수들의 평균

ds5 <- main.ds

ds5.agg <- aggregate(
   G ~ Tm,
   data = ds5,
   mean
)
result5 <- mean(ds5.agg$G)
print(result5)


# Q6. Tm의 값이 MIA이며 Pos는 C또는 PF인 선수의 MP값의 평균은?
ds6 <- main.ds

ds6.temp <- ds6 %>% filter(Tm == "MIA" & (Pos == "C" | Pos == "PF") )
head(ds6.temp)
result6 <- mean(ds6.temp$MP)
print(result6)


# Q7. 전체 데이터중 G의 평균값의 1.5배 이상인 데이터들만 추출했을때 3P값의 평균은?

ds7 <- main.ds
str(ds7)

mean <- mean(ds7$G)
m <- mean * 1.5

ds7.temp <- ds7 %>% filter( G >= m ) %>% select( c('X3P'))
ds7.temp
result7 <- mean(ds7.temp$X3P)
print(result7)

# 8. Age의 평균 이상인 그룹과 평균 미만인 그룹간의 G값의 평균의 차이는?

ds8 <- main.ds
m <- mean(ds8$Age)

g1 <- ds8 %>% filter(Age >= m)
g2 <- ds8 %>% filter(Age < m) 

g1 <- mean(g1$G)
g2 <- mean(g2$G)

result8 <- abs(g1 - g2)
print(result8)


# Q9. 평균나이가 가장 젊은 팀은 어디인가
# 각 팀별 평균 나이

ds9 <- main.ds
ds9.agg <- aggregate(
    data = ds9,
    Age ~ Tm,
    FUN = "mean"
)

ds9.agg <- ds9.agg[ order(ds9.agg$Age),]
result9 <- ds9.agg[1,]
result9$Tm <- as.character(result9$Tm)
result9$Tm
result9 <- result9$Tm
print(result9)


# Q10. Pos그룹별 평균 MP값을 구하여라

ds10 <- main.ds

ds10.agg <- aggregate(
    data = ds10,
    MP ~ Pos,
    FUN = "mean"
)

result10 <- ds10.agg
print(result10)
