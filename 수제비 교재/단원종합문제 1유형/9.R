library(dplyr)

main <- mtcars

# 수동(am=1) 중 에서 가장 무게(wt)가 작게 나가는 10개 데이터의 평균 연비(mpg)와
# 자동(am=0) 중에서 가장 무게(wt)가 적게 나가는 10개 데이터의 평균 연비(mpg)를 구하시오

main <- main[order(main$wt), ]
head(main, 10)

manual <- main %>% filter(am == 1) 
auto <- main %>% filter(am == 0)

manual <- manual$wt[1:10]
auto <- auto$wt[1:10]

