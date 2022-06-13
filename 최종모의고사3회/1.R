library(dplyr)

data(esoph)
main.ds <- esoph
head(main.ds)



# 환자 수(ncases)와 대조군 수(ncontrols)를 합한 새로운 컬럼인 관측자 수(nsums)를 생성하고,
main.ds$nsums <- NA
main.ds$nsums <- main.ds$ncases + main.ds$ncontrols

# 음주량과 흡연량에 따른 관측자 수(nsums)의 이원 교차표를 생성하여 확인하고
# nsums의 카이제곱값을 구하시오

tail(main.ds)

# 음주량과 흡연량에 따른 이원 교차표생성
tab <- xtabs(nsums ~ alcgp + tobgp, data = main.ds)
print(tab)
print(chisq.test(main.ds$nsums)[1])


# ChickWeight 데이터 세트이다.
# weight를 최소-최대 척도(Min-Max Scaling)으로 변환한 결과가 0.5이상인 레코드 수를 구하시오

data(ChickWeight)
main.ds <- ChickWeight

min_weight <- min(main.ds$weight)
max_weight <- max(main.ds$weight)

main.ds$sc_weight <- scale(
    main.ds$weight, 
    min_weight, 
    max_weight - min_weight
)

count <- main.ds %>% filter( sc_weight >= 0.5 )
print(nrow(count))

# 3번
library(mlbench)
data("PimaIndiansDiabetes2")
main.ds <- PimaIndiansDiabetes2
head(main.ds)
colSums(is.na(main.ds))
nrow(main.ds)

main.ds <- main.ds[ !is.na(main.ds$glucose),  ]
main.ds <- main.ds[ !is.na(main.ds$pressure) , ]
main.ds <- main.ds[ !is.na(main.ds$mass) , ]
colSums(is.na(main.ds))
nrow(main.ds)

# 나이를 조건에 맞게 그룹화

main.ds$age[main.ds$age >= 20 & main.ds$age <= 40] <- 1
main.ds$age[main.ds$age >= 41 & main.ds$age <= 59] <- 2
main.ds$age[main.ds$age >= 60 ] <- 3
main.ds$age <- as.factor(main.ds$age)


# 1그룹의 pos의 수 / 1그룹 수
g1 <- main.ds %>% filter(age == "1")
g1.pos <- g1 %>% filter(diabetes == "pos") %>% nrow()
g1 <- g1.pos / nrow(g1)
g1

# 2그룹의 pos의 수 / 2그룹 수
g2 <- main.ds %>% filter(age == "2")
g2.pos <- g2 %>% filter(diabetes == "pos") %>% nrow()
g2 <- g2.pos / nrow(g2)
g2

# 3그룹의 pos의 수 / 3그룹 수
g3 <- main.ds %>% filter(age == "3")
g3.pos <- g3 %>% filter(diabetes == "pos") %>% nrow()
g3 <- g3.pos / nrow(g3)
g3


result <- max(g1, g2, g3)
print(result)

# 다른 방법

main.ds2 <- PimaIndiansDiabetes2
main.ds2 <- main.ds2 %>% filter(
    !is.na(glucose) & !is.na(pressure) & !is.na(mass)
)

colSums(is.na(main.ds2))

main.ds2 <- main.ds2 %>% mutate(
    age_grp = ifelse(
        age >= 60, "3", ifelse (
            age >= 41 , "2", "1"
        )
    )
)


sum_ds2 <- main.ds2 %>% group_by(age_grp) %>% summarise(
    total_num = n(),
    diab_num = sum(diabetes == "pos"),
    ill_rate = diab_num/total_num
) %>% arrange(desc(ill_rate))

result <- head(sum_ds2$ill_rate, 1)
result
