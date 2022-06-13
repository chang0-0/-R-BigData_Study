library(dplyr)

# mtcars에서  4기통인 자동차의 비율을 구하시오
# 4기통 / 전체

data(mtcars)
str(mtcars)

result13 <- length(mtcars$cyl[mtcars$cyl == 4]) / nrow(mtcars)
print(result13)
