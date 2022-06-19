library(dplyr)
library(tidyr)

main <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/mobile/train.csv',
    encoding = 'UTF-8'
)


# Q1. price_range 의 각 value를 그룹핑하여 각 그룹의 n_cores 의 빈도가 가장높은 value와 그 빈도수를 구하여라

ds1 <- main
result1 <- ds1 %>% group_by(price_range, n_cores) %>% summarise(n = n()) %>% arrange(desc(n)) %>% slice(1)
print(result1)

# Q2. price_range 값이 3인 그룹에서 상관관계가 2번째로 높은 두 컬럼과 그 상관계수를 구하여라

