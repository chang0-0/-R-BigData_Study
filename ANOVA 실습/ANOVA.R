data(airquality)
df = airquality
head(df)
str(df)

# Month의 값이 유일값이 맞는지 확인
unique(df$Month)

aggregate(data = df, Temp ~ Month, FUN = "mean")
# 월별 평균 출력


# 처음 Month가 수치형으로 들어갔기 때문에 자유도가 1이 나오게 됨
# 그래서 factor로 바꾸거나 문자열로 넣어줘야 함
df[, "Month"] = as.character(df$Month)
model = aov(formula = Temp ~ Month, data = df)

# 녀summary를 해야 ANOVA테이블이 나옴.
summary(model)


model_sum <- summary(model)
str(model_sum)
model_sum[[1]][1, 4]
print(round(model_sum[[1]][1, 4], 3 ))

# model 자리에 ANOVA를 넣어야 함
TukeyHSD(model, which = "Month")
