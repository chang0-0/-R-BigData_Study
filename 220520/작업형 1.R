
library(dplyr)
library(mlbench)
data(BostonHousing)
ds <- BostonHousing
summary(ds)

# 결측값 없음
colSums(is.na(ds))

## crim항목의 상위에서 10번 째 값

# crim을 기준으로 내림차순으로 정렬
ds <- ds[c(order(-ds$crim)), ]

tenth <- head(ds$crim, 10)
tenth.10 <- tenth[10]
tenth.10

# 상위 10번째 값으로 상위 10개의 값을 변환
ds$crim[1:10] <- tenth.10
head(ds, 20)

# age가 80이상인 값에 대하여 crim의 평균

ds.80 <- ds %>% filter(age >= 80)
result <- mean(ds.80$crim)
print(result)