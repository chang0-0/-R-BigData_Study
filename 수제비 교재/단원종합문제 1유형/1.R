library(dplyr)

data(airquality)
main <- airquality

colSums(is.na(main))

# Solar.R에 결측값이 있는 행을 제거하고, 
# Ozone 항목의 결측값을 중앙값으로 대체한 후 중앙값으로 대체하기 이전과 이후의 Ozone의 
# 표준편차의 차이를 구하시오.

after <- main %>% filter( !is.na(Solar.R) )
before <- main %>% filter( !is.na(Solar.R) )

after[is.na(after$Ozone), ] <- median(after$Ozone, na.rm = T)

result1 <- abs(sd(after$Ozone) - sd(before$Ozone, na.rm = T))
print(result1)

