library(tidyr)
library(dplyr)
library(caret)
library(knn)

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/이기적 교재/최종모의고사 1회")
list.files()

# 대출 여부를 분류하는 가장 최적의 이웃의 크기값(k)을 구하고,
# 분류정확도를 산출하시오
# 추가적인 관리가 필요한 특성을 가진 사람들의 수를 계산하시오
# (트레이닝 7 : 테스트 3 의 비율로 트레이닝 데이터와 테스트 데이터로 구분하고, 
# 대출여부는 계층적으로 구분하는 샘플구분 방식을 따른다. 또한 Z값을 산출하여 정규화 한다. )

main.ds <- read.csv(
    file = "Bank_Personal_Loan_Modelling.csv",
    fileEncoding = 'UTF-8-BOM',
    stringsAsFactor = TRUE
)

summary(main.ds)
head(main.ds)
str(main.ds)

# 결측값 없음
sapply(main.ds, function(x) {
    sum(is.na(x))
})
