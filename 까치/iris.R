library(dplyr)
data(iris)
ds <- iris

summary(ds)
# 제 1분위 수 구하기 = quantile의 0.25 옵션
quantile(ds$Sepal.Width, 0.25)
summary(ds$Sepal.select)

ds

mean_len <- mean(ds$Sepal.Length)
mean_wid <- mean(ds$Sepal.Width)

print(mean_len + mean_wid)
