library(dplyr)

# airquality 데이터에서 Ozone의 결측값을 평균 값으로 대체하고,
# median 값에서 2 * IQR을 뺀 값과 median 값에서 2 IQR을 더한 값 사이에 존재하는
# Ozone값의 합계를 구하시오

data(airquality)
str(airquality)

main <- airquality
main[is.na(main$Ozone), "Ozone"] <- mean(main$Ozone, na.rm = T)

str(main)

lower <- median(main$Ozone) - (2*IQR(main$Ozone))
upper <- median(main$Ozone) + (2*IQR(main$Ozone))

main <- main %>% filter(Ozone > lower & Ozone < upper )
result17 <- sum(main$Ozone)
print(result17)