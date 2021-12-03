library(MASS)
library(dplyr)

data(Cars93)

df <- Cars93
# 결측값 확인
colSums(is.na(df))

# 데이터의 결측값을 중앙값으로 대체
df2 <- df %>% mutate(Luggage.room = ifelse(is.na(Luggage.room), median(Luggage.room, na.rm=TRUE), Luggage.room))

# 변환 전과 후의 평균의 차이
summary(df)
summary(df2)

df_mean <- mean(df$Luggage.room, na.rm=TRUE)
df2_mean <- mean(df2$Luggage.room)

print(df2_mean - df_mean)

# 책에서 나온 결측값 대체 방법
tdf$Luggage.room[is.na(Cars93$Luggage.room)] <- median(df$Luggage.room, na.rm=TRUE)

