data(mtcars)
ds <- mtcars
summary(ds)

# filter로 원하는 값을 출력
# 수동(am = 1) 중 -> (에서) -> 가장 무게(wt)가 작게 나가는 10개 데이터의 평균 연비(mpg)
# 자동(am = 0) 중에서 가장 무게(wt)가 적게 나가는 10개 데이터의 평균 연비(mpg) 차이를 구하시오.
ds_man_min <- ds %>% filter(am == 1) %>% arrange(wt) %>% head(10)
summary(ds_man_min)

# 평균 연비
mean_man_mpg <- mean(ds_man_min$mpg)

# 자동중에서 무게가 가장 적게나가는 10개 데이터의 평균 연비
ds_aut_min <- ds %>% filter(am == 0) %>% arrange(wt) %>% head(10)

ds_aut_min

mean_aut_mpg <- mean(ds_aut_min$mpg)
result <- mean_man_mpg - mean_aut_mpg
print(result)
