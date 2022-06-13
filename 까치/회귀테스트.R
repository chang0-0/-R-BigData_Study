age <- 18 : 29
age

height <- 70 : 81
height

plot(x=age, y=height)

# y축 키 , x축 나이
res = lm(height ~ age)

# lm해석

intercept (y절편) : 64.928
age (독립변수의 기울기) : 0.635
키 = 0.635 * 나이 + 64.928
height = 0.635 * age + 64.928

abline(res)

# install.packages("UsingR")
library(UsingR)
data(galton) ; str(galton)

par(mfrow = c(1,2))
hist(galton$child, col="coral")
hist(galton$parent, col = "lightpink")
par(mfrow = c(1,1))

out = lm(child ~ parent, data = galton)
summary(out)

library(ggplot2)
ggplot(data = galton, aes(x = parent, y = child)) + geom_count() + geom_smooth(method = "lm")
