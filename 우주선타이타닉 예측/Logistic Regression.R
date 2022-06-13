library(tidyverse)
library(caret)
library(randomForest)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/우주선타이타닉 예측")

train = read.csv("../우주선타이타닉 예측/train.csv")
test = read.csv("../우주선타이타닉 예측/test.csv")

head(test, 10)
head(train, 10)

test$Transported <- NA
all = rbind(train, test)
head(all ,3)

# PassengerId를 "-"를 기준으로 Id와 Num으로 2개로 나눔. 
temp = all$PassengerId %>% str_split(pattern = "_", simplify = TRUE)
all$IdGroup <- temp[, 1] %>% as.numeric()
all$IdNum <- temp[, 2] %>% as.numeric()

# Cabin도 3개로 나눔
temp = all$Cabin %>% str_split(pattern="/", simplify = TRUE)
all$CabinDeck = temp[,1] %>% as.factor()
all$CabinNum = temp[,2] %>% as.numeric()
all$CabinSide = temp[, 3] %>% str_replace(pattern="P", replace="Port") %>% str_replace(pattern="S", replace="StarBoard") %>% as.factor()

# Name을 성과 이름으로 분리함.

temp = all$Name %>% str_split(pattern = " " , simplify = TRUE)
all$FirstName = temp[, 1]
all$LastName = temp[, 2]

# 수정해서 원래 있던 컬럼들 제거

all = all[, !colnames(all) %in% c("PassengerId", "Cabin", "Name")]

# True False를 1 과 0으로 변환
all$CryoSleep = as.integer(all$CryoSleep == "True")
all$CryoSleep = as.integer(all$VIP == "True") 
all$Transported = as.integer(all$Transported == "True")

all %>% head(3)

# NA를 0으로 모두 채우기.
all$RoomService = all$RoomService %>% replace_na(0)
all$FoodCourt = all$FoodCourt %>% replace_na(0)
all$ShoppingMall = all$ShoppingMall %>% replace_na(0)
all$Spa = all$Spa %>% replace_na(0)
all$VRDeck = all$VRDeck %>% replace_na(0)

sapply(all, FUN = function(x) {
    sum(is.na(x))
})


all$Age = all$Age %>% replace_na(mean(all$Age, na.rm = TRUE))
all$CabinNum = all$CabinNum %>% replace_na(-1)


encoder = dummyVars(~ HomePlanet + Destination + CabinDeck + CabinSide, data=all, sep="") 
all.dummy = predict(encoder, newdata=all)
colnames(all.dummy)[5:8] = c("Destination", "DestinationC", "DestinationP", "DestinationT")
all.dummy %>% head(3)

# 예측한 값을 넣기 위해서 원래 있던 컬럼제거

factors = c("HomePlanet", "Destination", "CabinDeck", "CabinSide", "FirstName", "LastName")
all.factor = all[factors]
all = all[!colnames(all) %in% factors]

drop.cols = c("HomePlanet", "Destination", "CabinDeck", "CabinSide", "CabinSideStarBoard")
all = all[!colnames(all) %in% drop.cols]

# NA갯수 파악

# Transported가 없는 행의 결측값. 총 갯수.
train_na = apply( all[!is.na(all$Transported),], MARGIN = 2, FUN = function(x) {sum(is.na(x))} )

# Transorted가 있는 행의 결측값. 총 갯수
test_na = apply( all[is.na(all$Transported),], MARGIN = 2, FUN = function(x) {sum(is.na(x))} )


# train_na의 길이는 곧 train의 길이
temp = matrix(c(train_na, test_na), nrow = length(train_na), dimnames = list(names(train_na)))
data.frame(Data = c("TRAIN", "TEST"), t(temp))

# split dataframe into tow pats: train and test

train = all[!is.na(all$Transported), ]; train.factor = all.factor[!is.na(all$Transported), ]
test = all[is.na(all$Transported), ]; test.fa = all.factor[!is.na(all$Transported), ]

nfold = 4
set.seed(0)

cv.idx = split(sample(1:nrow(train)), 1:nfold)
str(cv.idx)

accuracy = function(actual, pred){sum(actual == pred) / length(actual)}
pred = rep(0, nrow(train))

for (i in 1:nfold){
    cv.train = train[-cv.idx[[i]],]
    cv.valid = train[cv.idx[[i]],]
    model = randomForest(Transported~., data=cv.train)
    pred[cv.idx[[i]]] = predict(model, newdata=cv.valid, type="response")
}

best.t = 0
best.acc = 0

for (t in seq(0.4, 0.6, 0.01)){
    pred.t = as.integer(t < pred)
    acc.t = accuracy(pred.t, train$Transported)
    if (acc.t > best.acc){
        best.acc = acc.t
        best.t = t
        }
}
cat("Best t :", best.t, "\nBest accuracy :", best.acc)

model = randomForest(Transported~., data=train)
summary(model)
pred = predict(model, newdata=test, type="response")
pred = as.integer(best.t < pred)

head(pred, 300)


sub = read.csv("sample_submission.csv")


sub$Transported = sapply(pred, FUN=function(x){if (x==1){"TRUE"} else {"FALSE"}})
write.csv(sub, "submission.csv", row.names=FALSE)
sub %>% head(3)

