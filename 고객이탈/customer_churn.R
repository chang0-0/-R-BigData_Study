library(dplyr)

setwd("C://Users//Samsung//Desktop//빅분기 실기 준비//고객이탈")

ds <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv", header = T, fileEncoding = "UTF-8-BOM")
summary(ds)

# 1. 결측치 제거
ds <- na.omit(ds)

library(caret)
idx <- createDataPartition(ds$Churn, p = 0.8)

x_train <- ds[idx$Resample1, ]
