# https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques

# https://www.kaggle.com/code/dawoon0901/r-t2-4-house-prices

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/House Prices")
list.files()

library(dplyr)
library(caret)
library(ModelMetrics)
library(randomForest)
library(readr)
library(tidyselect)

train <- read.csv(
    file = "train.csv",
    fileEncoding = 'UTF-8-BOM',
    na.strings = c("", "NA", NA, "na", " "),
    stringsAsFactor = TRUE
)

colSums(is.na(train))

head(train)
str(train)

price_which = which(
    colnames(train) == 'SalePrice'
)

id_which = which(
    colnames(train) == 'Id'
)


set.seed(2021)
idx <- sample(
    1:nrow(train),
    size = nrow(train) * 0.8
)

x_train <- train[idx, -price_which]; y_train <- train[idx, c(id_which, price_which)]
x_test <- train[-idx, -price_which]; y_test <- train[-idx, c(id_which, price_which)]

# x는 독립변수 데이터들로 예측할 SalePricee를 제외한 전부를 선택한다. 
# y는 종속변수로 Id와 예측할 값인 SalePrice 2개의 값만 선택한다.

dim(x_train); dim(y_train); dim(x_test); dim(y_test)

full <- left_join(
    y_train, 
    x_train, 
    by = 'Id'
)

colSums(is.na(full))
sum(is.na(full))
str(full)


# 결측값이 너무 많은 것들은 제거
full <- full %>% select(-c(PoolQC, Fence, MiscFeature, Alley, Id, FireplaceQu, LotFrontage))

# 결측값 처리
full$GarageType[is.na(full$GarageType)] <- "None"
full$GarageFinish[is.na(full$GarageFinish)] <- "None"
full$GarageQual[is.na(full$GarageQual)] <- "None"
full$GarageYrBlt[is.na(full$GarageYrBlt)] <- "None"

full$BsmtQual[is.na(full$BsmtQual)] <- "None"
full$BsmtCond[is.na(full$BsmtCond)] <- "None"
full$BsmtExposure[is.na(full$BsmtExposure)] <- "None"
full$BsmtFinType1[is.na(full$BsmtFinType1)] <- "None"
full$BsmtFinType2[is.na(full$BsmtFinType2)] <- "None"

full$MasVnrArea[is.na(full$MasVnrArea)] <- "None"
full$MasVnrType[is.na(full$MasVnrType)] <- "None"

# factor 형으로 변환

full$GarageType <- as.factor(full$GarageType)
full$GarageYrBlt <- as.factor(full$GarageYrBlt)
full$GarageFinish <- as.factor(full$GarageFinish)
full$GarageQual <- as.factor(full$GarageQual)
full$GarageCond <- as.factor(full$GarageCond)
full$PavedDrive <- as.factor(full$PavedDrive)
full$KitchenQual <- as.factor(full$KitchenQual)
full$HeatingQC <- as.factor(full$HeatingQC)
full$CentralAir <- as.factor(full$CentralAir)
full$SaleType <- as.factor(full$SaleType)
full$SaleCondition <- as.factor(full$SaleCondition)
full$Condition1 <- as.factor(full$Condition1)
full$Condition2 <- as.factor(full$Condition2)
full$BldgType <- as.factor(full$BldgType)
full$LandSlope <- as.factor(full$LandSlope)
full$LotConfig <- as.factor(full$LotConfig)
full$LandContour <- as.factor(full$LandContour)
full$Street <- as.factor(full$Street)
full$MSZoning <- as.factor(full$MSZoning)
full$LotShape <- as.factor(full$LotShape)
full$Utilities <- as.factor(full$Utilities)
full$Neighborhood <- as.factor(full$Neighborhood)
full$BsmtCond <- as.factor(full$BsmtCond)
full$BsmtQual <- as.factor(full$BsmtQual)
full$BsmtExposure <- as.factor(full$BsmtExposure)
full$BsmtFinType2 <- as.factor(full$BsmtFinType2)
full$Heating <- as.factor(full$Heating)
full$RoofStyle <- as.factor(full$RoofStyle)
full$RoofMatl <- as.factor(full$RoofMatl)
full$Exterior1st <- as.factor(full$Exterior1st)
full$Exterior2nd <- as.factor(full$Exterior2nd)
full$MasVnrType <- as.factor(full$MasVnrType)
full$ExterQual <- as.factor(full$ExterQual)
full$ExterCond <- as.factor(full$ExterCond)
full$HouseStyle <- as.factor(full$HouseStyle)

full <- na.omit(full)


# 다시 데이터 분할

pred_SalePrice <- full$SalePrice
dummy <- dummyVars('~. -SalePrice', full)
full <- data.frame(
    predict(
        dummy,
        full
    )
)


a <- ifelse( colnames(full) %in% colnames(x_test), 1, 0
)

inx <- createDataPartition(
    full$SalePrice,
    p = 0.7,
    list = FALSE
)

inx

train <- full[inx, ]
valid <- full[-inx, ]


set.seed(210)
model <- randomForest(
    train$SalePrice ~ .,
    train,
)



rmse(1, 1)
rmsle(1, 1)
