library(dplyr)
library(ggplot2)

data(Boston)
ds <- Boston

# 1. 통계와 결측값 확인 
summary(ds)
colSums(is.na(ds))

head(ds)


ds <- ds %>% arrange(desc(crim))
ds_crim10 <- ds$crim[10] 
print(ds_crim10)

ds$crim <- replace(ds$crim, ds$crim>=ds_crim10, value=ds_crim10)
head(ds$crim, n=20)

# age 80이상인 값에 대해 crim 평균을 구하시오.
ds_age80 <- ds %>% filter(age >= 80)
ds_age80_mean <- mean(ds_age80$crim)

print(ds_age80_mean)