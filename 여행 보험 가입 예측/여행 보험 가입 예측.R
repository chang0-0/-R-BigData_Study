library(dplyr)
library(randomForest)
library(caret)

getwd()

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/여행 보험 가입 예측")
ds <- read.csv("TravelInsurancePrediction.csv", header = TRUE, stringsAsFactor = TRUE)
ds <- ds[-1]
str(ds)

ds <- na.omit(ds)

sapply(ds, FUN = function(x) {
    sum(is.na(x))
})

ds$TravelInsurance <- as.numeric(as.character(ds$TravelInsurance))
ds$ChronicDiseases <- as.numeric(as.character(ds$ChronicDiseases))
ds$Age <- as.numeric(as.character(ds$Age))
ds$AnnualIncome <- as.numeric(as.character(ds$AnnualIncome))
ds$FamilyMembers <- as.numeric(as.character(ds$FamilyMembers))
ds$EverTravelledAbroad <- as.factor(ds$EverTravelledAbroad)
ds$FrequentFlyer <- as.factor(ds$FrequentFlyer)
ds$GraduateOrNot <- as.factor(ds$GraduateOrNot)

str(ds)

set.seed(2022)
index <- sample(2, nrow(ds), replace = TRUE ,prob = c(0.7, 0.3))
table(index)
train <- ds[index == 1, ]
test <- ds[index == 2, ]

ds.forest <- randomForest(TravelInsurance ~ . , data = train)
ds.forest

forest.frame <- data.frame(ds.forest )
forest.frame


ds.pred <- predict(ds.forest, newdata = test, type="prob")


table(ds.pred)










