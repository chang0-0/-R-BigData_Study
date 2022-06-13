library(dplyr)
library(ISLR)

# Carseats 데이터 세트에서 매출(Sales)의 이상값을 제외한 데이터를 훈련 데이터로 선정할 때 Age의 표준편차를 구하시오
# (이상값은 평균보다 1.5표준 편차 미만이거나 초과인 값으로 선정한다.)

data(Carseats)
main <- Carseats
colSums(is.na(main))

s <- sd(main$Sales)
m <- mean(main$Sales)
m

upper <- m + 1.5*s
lower <- m - 1.5*s
upper
lower


head(main$Sales)

str(main)
train <- main %>% filter(Sales < upper & Sales > lower) %>% summarise(sd = sd(Age))
train

