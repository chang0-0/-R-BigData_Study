library(tidyverse)
library(dplyr)
library(randomForest)
library(Hmisc)
library(skimr)
library(ggplot2)
library(caret)

windows(height = 7, width = 7)
setwd("C:/Users/Samsung/Desktop/빅분기실기준비/우주선타이타닉 예측")

train <- read.csv("train.csv", stringsAsFactors = TRUE, na.strings = c("", NA), sep =",", header = TRUE)
test <- read.csv("test.csv", stringsAsFactors = TRUE, na.strings = c("", NA), sep = ",", header = TRUE)

# 검증 데이터 불러오기
val <- read.csv("submission.csv")
head(val)


head(train)
str(train)
summary(train)

skim_without_charts(train)


# 결측값 확인
colSums(is.na(test))
colSums(is.na(train))

# Name 컬럼 제거
train <- subset(train, select = -c(Name, PassengerId) )
test <- subset(test, select = -c(Name, PassengerId) )
summary(train)

train$CryoSleep <- as.factor(train$CryoSleep)
test$CrypSleep <- as.factor(test$CryoSleep)

# 나이를 중앙 값과 평균을 비교해보니 
# 큰 차이가 없어서 평균을 사용하기로했음
median(train$Age)
mean.age <- round(mean(train$Age))


# 나이 결측값 평균 값으로 대체
train$Age <- impute(train$Age, mean)
test$Age <- impute(test$Age, mean)
train$Age <- as.numeric(train$Age)
test$Age <- as.numeric(test$Age)


# 나이데이터 factor형으로 변환해서 각 구간에 따른 생존 여부 파악하기
# train$Age_bins <- cut(train$Age, breaks = c(-Inf, 12, 18 ,60, Inf), 
#     labels = c("Child", "Adolescent", "Adult", "Senior")
# ) 
# test$Age_bins <- cut(test$Age, breaks = c(-Inf, 12, 18 ,60, Inf), 
#     labels = c("Child", "Adolescent", "Adult", "Senior")
# ) 

# 연령대별 생존 비율
# ggplot(train) + geom_bar(aes(Age_bins, fill = Transported), position = "dodge2")


# VIP의 평균 확인
str(train)
train$VIP <- as.factor(train$VIP)

# false가 훨씬 많음을 확인
table(train$VIP)



# ggplot(train)+geom_bar(aes(VIP,fill=Transported),position="dodge2")

train <- train %>% fill(Destination,Cabin,HomePlanet,CryoSleep, .direction="updown")
test <- test %>% fill(Destination,Cabin,HomePlanet,CryoSleep, .direction="updown")

# RoomService 컬럼확인
colSums(is.na(train))
train$RoomService <- as.numeric(train$RoomService)
train$RoomService[is.na(train$RoomService)] <- 0
train$FoodCourt[is.na(train$FoodCourt)] <- 0
train$ShoppingMall[is.na(train$ShoppingMall)] <- 0
train$Spa[is.na(train$Spa)] <- 0
train$VRDeck[is.na(train$VRDeck)] <- 0
train$HomePlanet <- as.factor(train$HomePlanet)
train$HomePlanet[is.na(train$HomePlanet)] <- "Earth"
train$Transported <- as.factor(train$Transported)
train$VIP[is.na(train$VIP)] <- "FALSE"


test$VIP[is.na(test$VIP)] <- "FALSE"
test$RoomService <- as.numeric(test$RoomService)
test$HomePlanet <- as.factor(test$HomePlanet)
test$RoomService[is.na(test$RoomService)] <- 0
test$FoodCourt[is.na(test$FoodCourt)] <- 0
test$ShoppingMall[is.na(test$ShoppingMall)] <- 0
test$Spa[is.na(test$Spa)] <- 0
test$VRDeck[is.na(test$VRDeck)] <- 0
test$Transported <- as.factor(test$Transported)
test$HomePlanet <- as.factor(test$HomePlanet)
test$HomePlanet[is.na(test$HomePlanet)] <- "Earth"



head(train)
str(train)


train_forest <- randomForest( Transported ~ . , data = train)
train_forest

df.forest.pred <- predict( train_forest, newdata = test)
























