
#pf() 함수 : 특정 F값에 대응되는 유의확률을 구할 수 있음
# F-value가 9.59 일 때,
pf(9.59, df1 = 1, df2 = 8, lower.tail = FALSE)
# df1 = 집단간 분산 df2 = 집단내분산
# 9.59 이상이 발생할 확률 (이상을 검사할 때는 lower.tail = FALSE 옵션)
 
# 특정확률에 대응되는 F값을 산출할 수 있다.
qf(0.05, df1=1 , df2=8, lower.tail = FALSE)
# 유의수준 0.05

 