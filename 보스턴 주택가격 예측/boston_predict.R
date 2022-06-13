## 참고 : https://www.kaggle.com/code/erikbruin/house-prices-lasso-xgboost-and-a-detailed-eda

library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)

getwd()
setwd("C:/Users/Samsung/Desktop/빅분기실기준비/보스턴 주택가격 예측")
test <- read.csv("test.csv")
train <- read.csv("train.csv")

# Id를 지우기 전에 미리 변수로 저장
test_labels <- test$Id
test$Id <- NULL
train$Id <- NULL

# test셋에 SalePrice를 모두 NA값으로 추가.
test$SalePrice <- NA
head(test, 4)

full <- rbind(train, test)

ggplot_grp <- ggplot(data=full[!is.na(full$SalePrice),], aes(x=SalePrice)) +
        geom_histogram(fill="blue", binwidth = 10000) +
        scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

summary(full$SalePrice)

# 전체 full데이터 셋에서 결측값이 있는 것들만, NAcol에 저장
NAcol <- which(colSums(is.na(full)) > 0)

# NAcol을 출력하는데, 오름차순으로 정렬해서 출력
sort(colSums(sapply(full[NAcol], is.na)), decreasing = TRUE)

cat("There are", length(NAcol), 'columns with missing values')

# Imputing missing data

# PoolQC 데이터 수정. 
full$PoolQC[is.na(full$PoolQC)] <- 'None'

Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5) 
full$PoolQC <- as.integer(revalue(full$PoolQC, Qualities))
table(full$PoolQC)

full$PoolQC[2421] <- 2
full$PoolQC[2504] <- 3
full$PoolQC[2600] <- 2


# Miscellaneous feature 수정

# 기타기능의 
full$MiscFeature[is.na(full$MiscFeature)] <- 'None'
full$MiscFeature <- as.factor(full$MiscFeature)


# Alley factor형으로 변환

full$Alley[is.na(full$Alley)] <- 'None'
full$Alley <- as.factor(full$Alley)

full$Fence[is.na(full$Fence)] <- 'None'
full$Fence <- as.factor(full$Fence)

full$FireplaceQu[is.na(full$FireplaceQu)] <- 'None'
full$FireplaceQu<-as.integer(revalue(full$FireplaceQu, Qualities))
table(full$FireplaceQu)

for (i in 1:nrow(full)){
        if(is.na(full$LotFrontage[i])){
               full$LotFrontage[i] <- as.integer(median(full$LotFrontage[full$Neighborhood==full$Neighborhood[i]], na.rm=TRUE)) 
        }
}



full$Functional[is.na(full$Functional)] <- names(sort(-table(full$Functional)))[1]
full$Functional <- as.integer(revalue(full$Functional, c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))

full$LotShape<-as.integer(revalue(full$LotShape, c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3)))
table(full$LotShape)


full$Electrical[is.na(full$Electrical)] <- names(sort(-table(full$Electrical)))[1]
full$Electrical <- as.factor(full$Electrical)


full$SaleType[is.na(full$SaleType)] <- names(sort(-table(full$SaleType)))[1]
full$SaleType <- as.factor(full$SaleType)
