library(dplyr)
library(MASS)

data(Boston)
main.ds <- Boston
head(main.ds)
str(main.ds)

summary(main.ds)
sapply(
    main.ds,
    function(x) {
        sum(is.na(x))
    }
)

# 결측값 없음

# 상위 50개의 medv 값을 상위 50개의 medv값의 최솟값으로 변환한 후 
main.ds <- main.ds[ order(-main.ds$medv),  ]
head(main.ds, 40)

t50 <- main.ds$medv[50]
t50

main.ds$medv[c(1:50)] <- t50
head(main.ds$medv, 52)

temp <- main.ds %>% filter(crim > 1) %>% summarise(mean = mean(crim))
result1 <- temp
print(result1)

# crim이 1을 초과하는 값에 대한 crim의 평균을 구하시오.