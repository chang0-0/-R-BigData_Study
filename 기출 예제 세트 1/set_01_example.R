library(dplyr)
library(lubridate)
getwd()

df_01 = read.csv("set_01_data_01.csv")
df_02 = read.csv("set_01_data_02.csv")


# 문제 1. 
# 평일 기준으로 오후 1시 casual의 평균대비 registered의 평균이 얼마나 많은지 비율을 산출하고 
# 반올림하여 소수점 둘째 자리까지 출력하시오.

head(df_01)

# lubridate 라이브러리를 사용해서 hour 함수 사용
# hour 컬럼추가
df_01$hour <- hour(df_01$datetime)
head(df_01)


# aggregate함수 사용
# aggregate : 합계, 총액

# dataframe
# aggreate(x, by, FUN)

# formula
# aggregate(formula, data, FUN) 


# .은 dotnotation이라고 함.
# hour를 제외한 전체가 종속변수가 됨
# hour를 기준으로 다른 모든 컬럼의 평균값 구하기


# dot notation 사용
df_01_agg = aggregate(data = df_01[, c("casual", "registered", "hour")], . ~ hour, FUN = "mean")

casual_13 = df_01_agg[df_01_agg$hour == 13, "casual"]
registered_13 = df_01_agg[df_01_agg$hour == 13, "registered"]
print(round(registered_13 / casual_13, 2))

# 문제2. 
# 데이터의 수집일 중 맑은 날의 비중을 백분율로 소수점 둘째 자리까지 출력하시오.
# 맑은 날은 weather가 1인 비중이 가장 많은 날을 기준으로 한다.
# 답안 예시 : 12.34


# 임시적으로 count에 1이 들어가 있는 변수를 새로 만듬
# 날짜와 날씨별로 몇개나 있는지의 총합을 계산해서 변수로 만듬
# 다른 말로 날짜와 날씨 별로 count의 총합을 계산함

df_02_agg = aggregate(data = cbind(df_02, count = 1), count ~ date + weather, FUN = "sum")
head(df_02_agg)

# 정렬
df_02_agg = df_02_agg[order(df_02_agg$date, df_02_agg$weather), ]
head(df_02_agg)
library(reshape2)

# fill은 빈값을 특정값으로 모두 채우는 함수
# 여기서는 0으로 모두 채움
df_02_cast = dcast(data = df_02_agg, date ~ weather, value.var = "count", fill = 0)
head(df_02_cast)

# weather의 값을 분리해서 컬럼으로 나열함
# weater의 값인 1, 2, 3, 4를 순서대로 나열해서 각 값의 count를 모두 나타냄.
# 없는 값은 0으로 채움

