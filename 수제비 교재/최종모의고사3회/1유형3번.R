library(mlbench)
library(dplyr)

data(PimaIndiansDiabetes2)
main.ds <- PimaIndiansDiabetes2
nrow(main.ds)

colSums(is.na(main.ds))
main.ds <- main.ds %>% filter( !is.na(glucose) & !is.na(pressure) & !is.na(mass)) 

# age를 조건에 맞게 그룹화 

main.ds$age[ main.ds$age >= 20 & main.ds$age <= 40] <- 1
main.ds$age[ main.ds$age >= 41 & main.ds$age <= 59] <- 2
main.ds$age[ main.ds$age >= 60 ] <- 3

main.ds$age <- as.factor(main.ds$age)
main.ds$age

# 발병률이 가장 높은 나이 그룹의 발병률을 구하시오
# 발병률 = diabets 중 pos의 수 / 전체 인원 수
str(main.ds)

temp1 <- main.ds %>% group_by(age) %>% summarise(total_num = n(), diab_num = sum(diabetes == 'pos'),
    ill_rate = diab_num / total_num)

result3 <- max(temp1$ill_rate)
print(result3)
