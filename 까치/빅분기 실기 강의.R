
# 경로 지정해서 CSV파일 불러오기

setwd("C:/Users/Samsung/Desktop/빅분기실기준비/CSV파일")
getwd()
list.files()

df <- read.csv("app_rating.csv")
df
head(df)
str(df)

q = quantile(df$rating_count_tot)
q

 # 0%가 최소값,100%가 최대값
        0%        25%        50%        75%       100%
   1117.00   13908.25   91248.00  155286.50 1126879.00

min(df$rating_count_tot)
max(df$rating_count_tot)

quantile(df$rating_count_tot, probs = 0.45)

q1 = quantile(df$rating_count_tot, probs = 0.25)
q3 = quantile(df$rating_count_tot, probs = 0.75)

q1
q3

iqr = IQR(df$rating_count_tot) # q3 - q1의 값과 동일
iqr

# Q1 - 1.5IQR
# Q3 + 1.5IQR

df_out = df[ ( df$rating_count_tot < (q1 - 1.5 * iqr) ) | ( df$rating_count_tot > (q3 + 1.5 * iqr) ), ]
df_out

# 조건이 길어져서 짧게 만들어줌
cond1 = df$rating_count_tot < (q1 - 1.5 * iqr)
cond2 = df$rating_count_tot > (q3 + 1.5 * iqr)

# 범위를 벗어나는 이상값이 들어있는 row만 출력
df_out = df[cond1 | cond2, ]
print(df_out)

# 이상치만 제거하고 남은 데이터프레임 출력
cond3 = df$rating_count_tot >= (q1 - 1.5*iqr)
cond4 = df$rating_count_tot <= (q3 + 1.5*iqr)

# cond3와 cond4를 동시에 만족하는 row를 출력
df_in = df[cond3 & cond4, ]
df_in

# 행의 수
nrow(df_in)

# 평균
stat_mean = mean(df$rating_count_tot)
# 표준 편차
stat_sd = sd(df$rating_count_tot)

stat_mean
stat_sd

# 예를 들어서 평균으로부터 1 표준편차 더 큰것 , 평균으로 부터 2 표준편차 더 큰 값을 이상치라고 한다 했을 때,
# 위와 똑같이 조건을 걸어서 사용할 수 있음

cond5 = df$rating_count_tot > (stat_mean + 2 * stat_sd)
cond6 = df$rating_count_tot < (stat_mean - 2 * stat_sd)

df_out2 = df[cond5 | cond6, ]

print(df_out)
print(df_out2)
