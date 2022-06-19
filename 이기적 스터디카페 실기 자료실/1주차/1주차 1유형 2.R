library(dplyr)
library(tidyr)
library(caret)


main <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/churn/train.csv',
    fileEncoding = 'UTF-8-BOM'
)


# Q1.  남성 이탈(Exited)이 가장 많은 국가(Geography)는 어디이고 이탈 인원은 몇명인가?

ds1 <- main
ds1 <- ds1 %>% filter(Gender == 'Male' & Exited == 1)
temp <- aggregate(
    Exited ~ Geography,
    ds1,
    sum
)

temp <- temp[order( -c(temp$Exited)), ]
print(temp[1,])


# Q2. 카드를 소유(HasCrCard ==1)하고 있으면서 활성멤버(IsActiveMember ==1) 인 고객들의 평균나이는? 

ds2 <- main
str(ds2)
result2 <- ds2 %>% filter(HasCrCard == 1 & IsActiveMember == 1) %>% summarise(mean = mean(Age))
print(result2)



# Q3. Balance 값이 중간값 이상을 가지는 고객들의 CreditScore의 표준편차를 구하여라
ds3 <- main
me <- median(main$Balance)
result3 <- ds3 %>% filter(Balance >= me) %>% summarise(sd = sd(CreditScore))
print(result3)
