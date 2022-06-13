library(mlbench)
data(BostonHousing)
ds <- BostonHousing

head(10)
# 1. crim 변수 상위 10개 값 출력

# 상위 10개 변환
ds$crim <- sort(ds$crim, decreasing = TRUE)
top10 <- ds$crim[10]
ds$crim[1:10] <- top10

over80 <- ds %>% filter(age >= 80)
mean(over80$crim)


