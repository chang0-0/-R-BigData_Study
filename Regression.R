library("Orange")
data("Orange")

str(Orange)
summary(Orange)

cor(Orange$circumference, Orange$age)

plot(Orange$circumference, Orange$age, col = "coral", pch = 19)

data("iris")

summary(iris)

cor.test(iris$Petal.Length, iris$Petal.Width, method = "pearson" ) # 피어슨 상관계수

head(Orange, n=10)

cor.test(Orange$circumference, Orange$age, method = "pearson") # 피어슨 상관계수
cor.test(Orange$circumference, Orange$age, method = "spearman") # 스피어만 상관계수

data(Orange)
head(Orange)
str(Orange)

print(Orange$circumference)
print(Orange$age)

model <- lm(circumference ~ age, Orange) # 선형 회귀 분석
model

# 잔차 
 r <- residuals(model)
 r[0:4]

 summary(model)

# fitted() 함수로 model이 예측한 값 구하기
f <- fitted(model)

# residuals() 함수로 잔차 구하기
r <- residuals(model)

# 예측한 값에 잔차를 더하여 실제값과 동일한지 확인해보자
# 예측한 값과 잔차 더하기

f[0:4] + r[0:4]

Orange[0:4, 'circumference']

summary(model)

# 상관계수
cor(Orange$circumference, Orange$age)

# 상관계수(r)의 제곱이 결정계수와 동일한 값이 출력되는지 확인
cor(Orange$circumference, Orange$age) ^ 2

plot(Orange$AGE, Orange$circumference)
abline(coef(model))
