library(dplyr)
library(ISLR)

data(Hitters)
main <- Hitters

str(main)
summary(main)

x <- lm(
    Salary ~ PutOuts, data = main
)

xtest <- summary(x)

str(xtest)
xtest$adj.r.squared
xtest$r.squared 
names(xtest$r.squared) <- "r.sq"


str(main)

main <- na.omit(main)
main <- Hitters

main <- na.omit(main)
colSums(is.na(main))


full <- lm(
    Salary ~ ., 
    data = main
)

summary(full)

fit <- step(full, direction = 'both')

library(car)
vif(fit) # 10이상일 경우 다중공선성이 심각

newmodel <- lm(
    Salary ~ Walks + Division + PutOuts,
    main
)

newmodel
aa <- summary(newmodel)
# Residual standard error: 391.3 on 259 degrees of freedom
# Multiple R-squared:  0.2561,    Adjusted R-squared:  0.2475
# F-statistic: 29.72 on 3 and 259 DF,  p-value: < 2.2e-16

glance(newmodel)


aa
str(aa)
aa$cov.unscaled

summary(newmodel)[12]

rmse(newmodel)
# > rmse(newmodel)
# [1] 388.3514

windows(width = 4, height = 4)
plot(newmodel)
abline(newmodel)

str(main)
plot(main$Salary ~ main$Walks)
abline(ce)

windows(width = 5, height = 5)

a <- lm(main$Salary ~ main$Walks)
a


plot(a)
plot(main$Salary, main$Walks, xlim = c(2000, 2020))
abline(a, col = 'red')
