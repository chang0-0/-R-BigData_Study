library(ISLR)
library(dplyr)

data(Carseats)
df <- Carseats
summary(df)

# 매출의 이상값 제외 
# 이상값은 평균보다 1.5 표준편차 이하이거나 이상인 값으로 선정한다.
df_sales_mean <- mean(df$Sales)
print(df_sales_mean)

ds_sales_sd <- sd(df$Sales)
print(df_sales_sd)

outlier_upper <- mean(df$Sales) + 1.5*sd(df$Sales)
outlier_lower <- mean(ds$Sales) - 1.5*sd(df$Sales)

# 이상값을 제외하고 Age표준편차 구하기
df_age <- df %>% filter(Sales <= outlier_upper & Sales >= outlier_lower) %>% summarise(sd = sd(Age))

# 최종 결과 출력
print(df_age)
