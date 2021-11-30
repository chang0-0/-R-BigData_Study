customer <- c('kim', 'lee', 'park', 'choi', 'seo')
deposit <- c(5000000, 4500000, 4000000, 5500000, 6000000)
rate <- c(3.5, 3, 4, 5, 4.5)
period <- c(2, 2, 5, 7, 4)

names(deposit) <- customer
names(rate) <- customer
names(period) <- customer

who <- customer

year <- deposit[who] * (1 + rate[who]/100)^period[who] - deposit[who]

sum <- deposit[who] * (1 + rate[who]/100)^period[who]

# kim의 경우 연간 1.035의 이자를 가짐
print(who)

c(who, '의 적금 만기 ', sum, - deposit[who], '복리', sum,'\n')


boklee <- deposit[who]

for(who in customer) {
  cat('고객', who, '기간', period[who],'복리', sum[who] - deposit[who], '만기 금액', sum[who], '입니다.', '\n')
}
