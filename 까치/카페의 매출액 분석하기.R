espresso <- c(4, 5, 4, 6, 5, 4, 7)
americano <- c(63, 68, 64, 68, 72, 89, 94)
latte <- c(61, 70, 59, 71, 71 ,92, 88)

sale.espresso <- 2 * espresso
sale.almericano <- 2.5 * americano
sale.latte <- 3 * latte

sale.day <- sale.espresso + sale.almericano + sale.latte
names(sale.day) <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
sale.day

sum(sale.day)

sale.mean <- mean(sale.day)

names(sale.day[sale.day >= sale.mean])

# 시험 출제 가능성.
# 매출이 가장 작은 요일, 날짜 찾기

sale.min <- min(sale.day)
sale.min

names(sale.day[sale.day ==sale.min])

sort(sale.day)