library(dplyr)
library(MASS)

# 심장의 무게(Hwt)의 이상값의 평균을 구하시오.
# 이상값은 평균에서 1.5 표준편차를 벗어나는 값으로 한다.

data(cats)
main <- cats

str(main)
head(main)

me <- mean(main$Hwt, na.rm = T)
msd <- sd(main$Hwt, na.rm = T)

upper <- me + 1.5*msd
upper

lower <- me - 1.5*msd
lower

# 이상값을 출력
temp <- main %>% filter(Hwt > upper | Hwt < lower) %>% summarise(mean = mean(Hwt))
result7 <- temp
print(result7)