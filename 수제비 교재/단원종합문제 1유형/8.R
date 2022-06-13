library(dplyr)
library(tidyr)
library(faraway)

main <- orings
str(main)
head(main, 10)
summary(main)

# damage가 1 이상일 경우의 temp와 damage의 피어슨 상관 계수를 구하시오.

main <- main %>% filter(damage >= 1)
result8 <- cor(main$temp, main$damage, method= 'pearson')
print(result8)

