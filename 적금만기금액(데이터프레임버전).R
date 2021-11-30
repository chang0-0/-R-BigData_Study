# 적금 데이터를 데이터 프레임으로 만들고
# 고객의 이름을 입력받아서 
# 입력받은 회원의 적금 만기 금액을 계산해서 출력하라.

library(svDialogs)

customer <- c('kim', 'lee', 'park', 'choi', 'seo')
deposit <- c(5000000, 4500000, 4000000, 5500000, 6000000)
rate <- c(3.5, 3, 4, 5, 4.5)
period <- c(2, 2, 5, 7, 4)


DF <- data.frame(customer, deposit, rate, period)
DF

who.input <- dlgInput('Input who')$res
who.input

who <- subset(DF, customer==who.input)
who

str(who)

year <- who$deposit * (1 + who$rate/100)^who$period - who$period
year

sum <- who$deposit * (1 + who$rate / 100)^who$period
sum

c(who$customer, '님의 기간은 ', who$period,'년 이고 복리는', sum-who$deposit,'입니다. 따라서 총 만기 금액은',sum,'입니다.')


