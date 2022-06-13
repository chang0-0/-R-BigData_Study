
data(cars)

# 제동 거리 예측
summary(cars)
str(cars)
head(cars, 10)

plot(cars$speed, cars$dist, main="cars")
cor.test(cars$speed, cars$dist)


a <- lm(cars$dist ~ cars$speed)
plot(cars$speed, cars$dist, main="cars")
abline(coef(a), col = 'red')

data(cars)
(a <- lm(dist ~ speed, cars))
predict(a, newdata=data.frame(speed=20))

data(Orange)
head(Orange)
str(Orange)

model <- lm(circumference ~ age, Orange)
coef(model)


f <- fitted(model)
r <- residuals(model)

deviance(model)

predict.lm(model, newdata = data.frame(
    age = 100
))

sm <- summary(model)
sm$adj.r.squared

data(mtcars)
model <- lm(mpg ~ ., data = mtcars)
new_model <- step(
    model, 
    direction = 'both'
)

form <- formula(mpg ~ wt + qsec + am)

model <- lm(form, mtcars)
model
summary(model)
plot(model)
windows(height = 5, width = 5)

library(ModelMetrics)
rmse(model)
