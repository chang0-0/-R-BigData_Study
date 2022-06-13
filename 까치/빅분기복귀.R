library(mtcars)

data(mtcars)
ds <- mtcars

m1 <- lm(hp~., data=mtcars)

m2 <- step(m1, direction = "backward")
m2

