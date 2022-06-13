library(dplyr)
library(caret)
library(MASS)


# Cars93 데이터셋의 Luggage.room의 결측값을 중앙값으로 변환한 후 변환 전, 후 평균 차이의 절댓값을 구하시오
data(Cars93)
main <- Cars93

main$Luggage.room[is.na(main$Luggage.room)] <- median(main$Luggage.room, na.rm = T)

head(main$Luggage.room, 100)

after <- mean(main$Luggage.room, na.rm = T)
before <- mean(Cars93$Luggage.room, na.rm = T)

result2 <- abs(after - before)
print(result2)