library(dplyr)

setwd("C://Users//Samsung//Desktop//빅분기 실기 준비//Covid19_TimeAge")
list.files()
df <- read.csv("./TimeAge.csv")

summary(df)
head(df)

# 항상 결측값이 있는지 먼저 확인
colSums(is.na(df))


# 연령(age)가 20대(20s)인 확진자(confirmes)의 평균과 
# 50대(50s)인 확진자(confirmed) 평균의 차이를 구하시오.

con_20 <- df %>% filter(age == "20s") 
con_20_mean <- mean(con_20$confirmed)
print(con_20_mean)

con_50 <- df %>% filter(age == "50s") 
con_50_mean <- mean(con_50$confirmed)
print(con_50_mean)

print(abs(con_20_mean - con_50_mean))
