# install.packages("ISLR")
library(ISLR)
library(gm)
data("Hitters")
ds <- Hitters
summary(ds$Salary)

gm(ds$Salary)

min_sal <- median(ds$Salary, na.rm = TRUE) - (2* IQR(ds$Salary, na.rm = TRUE))
max_sal <- median(ds$Salary, na.rm = TRUE) + (2* IQR(ds$Salary, na.rm = TRUE))

min_sal
max_sal

library(dplyr)

print(min_sal < ds$Salary)
print(max_sal > ds$Salary)


ds2 <- ds %>% filter(Salary < min_sal | Salary > max_sal)
result <- sum(ds2$Salary)
print(result)
