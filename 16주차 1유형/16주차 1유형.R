library(dplyr)
library(tidyr)

main.ds <- read.csv(
    "https://raw.githubusercontent.com/Datamanim/datarepo/main/pok/Pokemon.csv",
    encoding = "UTF-8",
    stringsAsFactor = TRUE,
    header = TRUE
)



# 1번 Legendary 컬럼은 전설포켓몬 유무를 나타낸다.
# 전설포켓몬과 그렇지 않은 포켓몬들의 HP평균의 차이를 구하여라

ds1 <- main.ds
head(ds1)

Noleg <- ds1 %>% filter(Legendary == 'False') %>% summarise(mean = mean(HP))
Yesleg <- ds1 %>% filter(Legendary == 'True')  %>% summarise(mean = mean(HP))
result1 <- Yesleg - Noleg
print(result1)


# 2. Type 1은 주속성 Type 2 는 부속성을 나타낸다. 가장 많은 부속성 종류는 무엇인가?

ds2 <- main.ds
ds2$count <- 1
result2 <- aggregate(
    data = ds2,
    count ~ Type.2,
    FUN = sum
)
result2 <- result2[ order(-result2$count), ]
result2$Type.2 <- as.character(result2$Type.2)
result2 <- result2$Type.2[2]
print(result2)


# 3. 가장 많은 Type 1 의 종의 평균 Attack 을 평균 Defense로 나눈값은?

ds3 <- main.ds
name <- ds3$Type.1[which.max(ds3$Type.1)]
name <- as.character(name)
name

ds3.temp <- ds3 %>% filter(Type.1 == name) %>% summarise(result = mean(Attack) / mean(Defense))
print(ds3.temp)


# 4. 포켓몬 세대(Generation) 중 가장많은 Legendary를 보유한 세대는 몇세대인가?
ds4 <- main.ds

ds4$count <- 1

# 세대별 전설의 개수
ds4.temp <- aggregate(
    data = ds4,
    count ~ Legendary + Generation,
    FUN = sum
)

ds4.temp <- ds4.temp %>% filter(Legendary == "True")
print(ds4.temp$Generation[ which.max(ds4.temp$count) ])


# 5. 'HP', 'Attack', 'Defense', 'Sp. Atk', 'Sp. Def', 'Speed' 
# 간의 상관 계수중 가장 절댓값이 큰 두 변수와 그 값을 구하여라

ds5 <- main.ds
ds5 <- ds5 %>% select(c('HP', 'Attack', 'Defense', 'Sp..Atk', 'Sp..Def', 'Speed'))
cor(ds5)

x_names <- names(ds5)
cor(ds5$HP, ds5$Attack)

cor.frame <- data.frame()
count <- 1
for(i in 1:length(ds5)) {

    for(j in 1:length(ds5)) {
        temp <- cor(ds5[, x_names[i]]  , ds5[ ,x_names[j]])[1]

        # 1제외 총 30개
        if(temp < 1) {
            cor.frame[count, 1] <- x_names[i]
            cor.frame[count, 2] <- x_names[j]
            cor.frame[count, 3] <- temp
        }

        count <- count + 1
    }
}

cor.frame <- na.omit(cor.frame)
names(cor.frame) <- c("v1", "v2", "cor")
result5 <- cor.frame[ order( -cor.frame$cor), ]
result5 <- result5[1,]
print(result5)


# 6. 각 Generation의 Attack으로 오름차순 정렬시 
# 상위 3개 데이터들(18개)의 Attack의 전체 평균을 구하여라

ds6 <- main.ds
ds6$Generation <- as.factor(ds6$Generation)
ds6 <- ds6[ order(ds6$Generation, ds6$Attack), ]

le <- levels(ds6$Generation)

ds6$Attack[ds6$Generation == 4]

ds6.temp <- data.frame()
for(i in 1:length(le)) {
    temp <- head(ds6$Attack[ ds6$Generation == i], 3)

    ds6.temp[i, 1] <- temp[1]
    ds6.temp[i, 2] <- temp[2]
    ds6.temp[i, 3] <- temp[3]

} 

sum(ds6.temp)
