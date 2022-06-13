
library(plyr)

ds <- read.csv("train.csv", stringsAsFactor = TRUE, header = TRUE)
colSums(is.na(ds))

# 전체에서 결측값 비율을 구하여라.
n <- nrow(ds)
n
dt.cs <- colSums(is.na(ds) / n)
dt.cs

dt.cs <- sort(dt.cs, decreasing = TRUE)
dt <- dt.cs[1]
result <- names(dt)
print(result)


