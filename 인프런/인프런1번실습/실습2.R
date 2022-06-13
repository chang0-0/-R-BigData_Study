library(dplyr)
library(randomForest)
library(lubridate)
library(reshape2)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/인프런1번실습")
list.files()

ds1 <- read.csv("data1.csv")

#### [[ 작업형 제 1유형 ]] ####
# Q1. 평일 기준으로 오후 1시 casual의 평균대비 registered의 평균이 얼마나 많은지
# 비율을 산출하고 반올림하여 소수점 둘째 자리까지 출력하시오.
# 답안 예시: 1.23

# 오후 1시 casual의 평균

ds1$hour <- hour(ds1$date)
head(ds1)

# aggregate함수 formula 사용
# 종속함수 별 독립변수 값을 얻을 수 있음

ds_agg <- aggregate(data = ds1[ , c("casual" , "registered", "hour")], . ~ hour, FUN = mean)

pm1 <- ds_agg %>% filter(hour == 13)
pm1

casual <- pm1$casual
regis <- pm1$registered

# casual 평균 대비 registered의 평균이 얼마나 많은지.
# casual에 비해서 register -> casual이 분모
result1 <- round( regis / casual, 2 )
print(result1)

# Q2. 데이터 수집일 중 맑은 날의 비중을 백분률로 소수점 둘째 자리까지 출력하시오.
# ※ 맑은 날은 weather가 1인 비중이 가장 많은 날을 기준으로 한다.
# 답안 예시: 12.34

ds2 <- read.csv("data2.csv")
head(ds2)
# waether 컬럼이 1이면 맑은 날

ds2 <- cbind(ds2, count = 1) # 1로 채워진 count 컬럼을 임시로 추가함
ds2_agg <- aggregate(data = ds2 , count ~ date + weather, FUN = sum)

head(ds2_agg,10)

# long데이터를 wide데이터로 수정
ds2_agg <- ds2_agg[order(ds2_agg$date, ds2_agg$weather ), ]
head(ds2_agg, 10)

# data = ds2_agg
# date ~ weather : 날짜를 종속변수(기준으로) 날씨 (weather)를 나열, 
# value.var = "count" : 분리할 컬럼지정(count), 생략가능, 
# fill = 0 없는 값은 0으로 채우고

# 해석 : ds2_agg 데이터에서 날짜별로 날씨를 구분한다, 분리할 컬럼은 count이다.
ds_02_case <- dcast( data = ds2_agg, date ~ weather, value.var = "weather", fill = 0 )


# date(날짜) 별로 weather(날씨)의 count를 보여준다.
# count는 각 weahter 값의 개수
ds2_wide <- dcast( data = ds2_agg, date ~ weather, value.var = "count", fill = 0 )
head(ds2_wide, 10)

ds2_wide[, "weather"] = apply(ds2_wide[, -1], MARGIN = 1, FUN = "which.max")
head(ds2_wide)

prop.table(table(ds2_wide$weather))

install.packages("xgboost")
library(xgboost)
library(mlbench)

data(PimaIndiansDiabetes2)
ds <- PimaIndiansDiabetes2
head(ds)

train.label <- as.integer(ds$diabetes) - 1

# diabetes 컬럼 제거
mat_train.data <- as.matrix(ds[, -9])
mat_test.data <- as.matrix(ds[, -9])

# train데이터와 test데이터 생성
xgb.train <- xgb.DMatrix( data = mat_train.data, label = train.label )
xgb.test <- xgb.DMatrix( data = mat_test.data )

head(xgb.train)
head(xgb.test)

param_list <- list(
    booster = "gbtree",
    eta = 0.001, max_depth = 10, gamma = 5, subsample = 0.8, colsample_bytree = 0.8,
    objective = "binary:logistic", eval_metric = "auc")

head(param_list)
