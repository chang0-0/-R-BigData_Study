# kaggle : https://www.kaggle.com/competitions/spaceship-titanic/data?select=test.csv
# 참고 : https://www.kaggle.com/code/kumarharsh24/spaceship-titanic-randomforest-r

library(dplyr)
library(ggplot2)
library(plyr)

# 데이터 설명
# Passenger ID : 승객 고유 번호

# HomePlanet : 승객이 출발한 행성, 승객의 거주 행성

# CryoSleep : 항해 기간의 승객이 극저온 수면 상태 여부

# Cabin : 승객이 머물고 있는 객실 번호, 갑판/숫자/측면 형태를 취합니다.
# 여기서 측면은 좌현의 경우 P, 우현의 경우 S가 됩니다.

# Age : 나이

# VIP : 승객이 VIP서비스를 받는지에 대한 여부

# RoomService, FoodCourt, ShoppingMall, Spa, VRDeck : 해당 서비스에 승객이 지불한 금액

getwd()
setwd("C:/Users/Samsung/Desktop/빅분기실기준비/우주선타이타닉 예측/")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

test$Transported <- NA
all <- rbind(train, test)

# all데이터에서 Transported의
ggplot(data=all[!is.na(all$Transported),], aes(x=Transported)) +
        geom_bar(fill= Transported)

NAcol <- which(colSums(is.na(all)) > 0)
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)

# levels가 일치하지 않아서 통일.
table(all$VIP)
all$VIP <- revalue(all$VIP, replace = c("False" = "FALSE", "True" = "TRUE"))


str(train)
str(test)
train$HomePlanet <- as.factor(train$HomePlanet)
test$HomePlanet <- as.factor(test$HomePlanet)

sapply(train, FUN = function(x) {
    sum(is.na(x))
})

sapply(test, FUN = function(x) {
    sum(is.na(x))
})

# VIP에 속한 사람중, roomservice를 사용한 사람
train$VIP <- as.factor(train$VIP)
test$VIP <- as.factor(test$VIP)

vip_roomservice <- train %>% filter(VIP == "True") %>% select(RoomService)
vip_zero <- train %>% filter(VIP == "True") %>% select(RoomService)




vip_zero <- vip_zero %>% filter(RoomService == 0) %>% count(RoomService)
vip_zero


train$CryoSleep <- as.factor(train$CryoSleep)
test$CryoSleep <- as.factor(test$CryoSleep)

table(test$CryoSleep)
train$Destination <- as.factor(train$Destination)
test$Destination <- as.factor(test$Destination)

