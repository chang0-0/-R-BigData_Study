library(dplyr)
library(ISLR)

library(dplyr)
data(Hitters)
main <- Hitters
head(main)
str(main)

main$Salary

# 이상값은 IQR의 2배를 초과하는 값
quan <- quantile(main$Salary, na.rm = T)
summary(main$Salary)

IQR(main$Salary, na.rm = TRUE)

# 중위수에서 IQR의 2배를 초과하는 값
lower <- median(main$Salary, na.rm = T) - (IQR(main$Salary, na.rm = T)*2)
upper <- median(main$Salary, na.rm = T) + (IQR(main$Salary, na.rm = T)*2)

lower
upper

result <- main %>% filter(Salary < lower | Salary > upper) %>% summarise( sum = sum(Salary))
print(result)
