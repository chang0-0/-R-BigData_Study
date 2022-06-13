library(dplyr)
library(Hmisc)

# 뉴욕의 공기 오염도를 측정한 airquality 데이터 세트이다.
# 데이터의 순서대로 90% 데이터를 훈련 데이터로 추출하고
# Ozone 항목의 결측값을 평균으로 변경한 후 변경 전 , 후의 중앙값의 차이를 구하시오

data(airquality)
main <- airquality

str(main)
n <- nrow(main) * 0.9
n
nrow(main)

train1 <- main[1:n, ]
train2 <- main[1:n, ]
nrow(train)

colSums(is.na(train1))

train1$Ozone[is.na(train1$Ozone)] <- mean(train1$Ozone, na.rm = T)
colSums(is.na(train1))

t1 <- median(train1$Ozone)
t2 <- median(train2$Ozone, na.rm = T)

result4 <- abs(t1 - t2)
print(result4)