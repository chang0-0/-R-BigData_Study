library(dplyr)
setwd("C:/Users/Samsung/Desktop/빅분기실기준비/타이타닉 전처리")

ds <- read.csv("train.csv")

temp <- sapply(ds, FUN = function(x) {
    sum(is.na(x))
})
temp
temp <- as.data.frame(temp)

tempn <- nrow(ds)

# 변수별 결측값의 비율
dt_cs <- colSums( is.na(ds) / tempn)
dt_cs <- as.data.frame(dt_cs)
dt_cs

names(dt_cs)[1] <- c("ratio")
str(dt_cs)
dt_cs

dt_cs <- arrange(dt_cs, desc(ratio))
result <- rownames(dt_cs)[1]
print(result)
 

 # 연도별 결핵 감염에 대한 유병률

 