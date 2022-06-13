library(dplyr)

data(iris)

Lm <- mean(iris$Sepal.Length)
Wm <- mean(iris$Sepal.Width)

result12 <- Lm + Wm
print(result12)
