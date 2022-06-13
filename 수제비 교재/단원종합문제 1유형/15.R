library(dplyr)
library(MASS)

# crim 항목이 1보다 작거나 같은 경우에 medv 항목의 mean값을 구하시오.

data(Boston)
str(Boston)

result15 <- Boston %>% filter(crim <= 1) %>% summarise(mean = mean(medv))
print(result15)
