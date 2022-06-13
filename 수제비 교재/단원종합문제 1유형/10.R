library(dplyr)
library(ggplot2)

# dimonds의 데이터를 순서대로 80% 데이터를 제거한 후 cut "Fail"이면서 
#carat이 1 이상인 diamonds의 price의 최댓값을 구하여라

data(diamonds)
str(diamonds)
head(diamonds)
summary(diamonds)
nrow(diamonds)

n <- nrow(diamonds) * 0.8

main <- diamonds[-c(1:n), ]

result10 <- main %>% filter(cut == 'Fair' & carat >= 1) %>% summarise(max = max(price))
print(result10)
