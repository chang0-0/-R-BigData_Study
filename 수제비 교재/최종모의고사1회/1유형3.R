library(dplyr)
data(mtcars)

# wt컬럼을 최소 최대 척도(min-max scale)로 변환한 후 0.5보다 큰 레코드 수를 구하시오.

normal = function(x) {
    (x - min(x)) / (max(x) - min(x))
}

mtcars$wt <- normal(mtcars$wt)

result3 <- mtcars %>% filter( wt > 0.5 ) %>% summarise( n =n())
print(result3)
