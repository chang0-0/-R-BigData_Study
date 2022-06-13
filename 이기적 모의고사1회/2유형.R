library(dplyr)
data(cars)
main.ds <- cars
head(main.ds)

# 제동거리와 스피드에 대한 선형회귀 예측모형을 만들고
# 이에 따른 회귀식과 회귀식이 표현된 시각화결과를 도출하시오.

str(main.ds)
windows(height = 4, width = 4)
plot(main.ds$speed, main.ds$dist)
cor.test(main.ds$speed, main.ds$dist)

cor(main.ds, method = "spearman")


car.lm <- lm(data=main.ds, dist ~ speed)
car.lm

summary(car.lm)
plot(main.ds$speed, main.ds$dist)
abline(coef(car.lm))
(car.lm <- lm(dist ~ speed, main.ds))
predict(car.lm , newdata = data.frame(speed = 20))
