library(dplyr)

# mtcars 데이터에서 
# 변속 기어 수가 4이고 수동 변속기인 데이터에서 자동차 연비의 mean값과 전체 마력의 표준편차의 합계를 구하시오

data(mtcars)
str(mtcars)

result14 <- mtcars %>% filter(gear == 4 & am == 1) %>% summarise(result = mean(mpg) + sd(hp))
print(result14)
