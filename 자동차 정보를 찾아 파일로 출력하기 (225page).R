setwd('C:\\Users\\Samsung\\Desktop\\source\\Ch06')

library(svDialogs)
library(xlsx)

carprice.new <- read.csv('carprice.csv', header=T)
str(carprice.new)

# 각 조건을 입력받아 변수에 저장한다. 입력값은 순서대로 Sporty, 5로 합니다.

input.type <- dlgInput('Input type')$res
input.city <- dlgInput('Input MPG.city')$res
input.city <- as.numeric(input.city)

# input.type이 Sporty이고, MPG.city가 5이상인 값들을 result 변수에 저장
result <- subset(carprice.new, Type == input.type & MPG.city >= input.city)
result

print(result)
sink('search.txt', append=T)
print(result)
sink()

# 새로운 변수를 검색해서 추가하기 input.type == Compact, input.city == 20
input.type <- dlgInput('Input type')$res
input.city <- dlgInput('Input MPG.city')$res

result <- subset(carprice.new, Type == input.type & MPG.city >= input.city)

print(result)
sink('search.txt', append=T)
print(result)
sink()
