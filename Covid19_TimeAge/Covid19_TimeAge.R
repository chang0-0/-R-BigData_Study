library(dplyr)
setwd("C:/Users/Samsung/Desktop/빅분기실기준비/Covid19_TimeAge")

ds <- read.csv("TimeAge.csv")

summary(ds)
head(ds)


# 항상 결측값이 있는지 먼저 확인
colSums(is.na(ds))

# 20대인 확진자 confirmed의 평균
str(ds)

c20 <- ds %>% filter(age == "20s")
mean(c20$confirmed)

c50 <- ds %>% filter(age == "50s")
mean(c50$confirmed)
