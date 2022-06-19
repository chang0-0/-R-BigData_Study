library(dplyr)
library(caret)


setwd('C:/Users/Samsung/Desktop/빅분기실기준비/수제비 교재/기출3회')
list.files()
main <- read.csv(
    file = 'train.csv',
    encoding = 'UTF-8',
)

tot <- nrow(main)
temp <- colSums(is.na(main)) / tot
result1 <- names(temp[which.max(temp)])
print(result1)


