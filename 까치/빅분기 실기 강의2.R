# 결측치 제거

df1 = airquality
df2 = airquality

is.na(c(2, NA, 5))

# Ozone이 결측인 row를 뽑아내라

head(df1)
df1[is.na(df1$Ozone) & is.na(df1$Solar.R), ]

na.omit(df1)

nrow(df1[is.na(df1$Ozone), ])
sum(is.na(df1$Ozone))

# 오존의 결측치가 없는 행을 추출
# 둘다 가능
df1_obs = df1[is.na(df$Ozone) == FALSE]
df1_obs = df1[!is.na(df1$Ozone), ]
head(df1_obs)

# 사용자 함수를 만들어서 결측치의 숫자를 파악
sapply(df1_obs, FUN = function(x) {sum(is.na(x))})

head(df2)
nrow(df2)

df2_obs = na.omit(df2)
nrow(df2_obs)

head(df2)

# 특정 칼럼의 NA만 값을 수정, 컬럼명을 적지않으면 NA인 행 전체가 0으로 처리됨
## 매우 중요
df2[is.na(df2$Ozone), "Ozone"] = 0
head(df2)

# 결측값을 평균값으로 대체
# 결측치를 제외하고 평균값을 계산
mean(df2$Solar.R, na.rm = TRUE)

df2[is.na(df2$Solar.R), "Solar.R"] = mean(df2$Solar.R, na.rm = TRUE)
head(df2)
