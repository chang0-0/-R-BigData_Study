library(dplyr)
library(caret)

main.ds <- iris
str(main.ds)

n <- nrow(main.ds) * 0.7
main.ds <- iris[c(1:n), ]
nrow(main.ds)

# 꽃 받침 길이 = Sepal.Length
sd(main.ds$Sepal.Length)


# 3번 

normal = function(x) {
    (x - min(x)) / (max(x) - min(x))
}

data(mtcars)
main.ds <- mtcars

main.ds$wt <- normal(main.ds$wt)

temp <- main.ds %>% filter(wt > 0.5) %>% summarise(length = n())
print(temp)