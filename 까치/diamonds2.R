library(ggplot2)
library(dplyr)

data(diamonds)
ds <- diamonds
summary(ds)
str(ds)

ds <- as.data.frame(ds)
 
# 80%데이터를 제거
sn <- sample(1: nrow(ds), size = nrow(ds)*0.8)
print(sn)

train <- ds[sn, ]
test <- ds[-sn, ]

print(train)
summary(test)

dim(test)

test_filter <- test %>% filter(cut == "Fair" & caret >= 1)
result <- max(test_filter$price)
print(result)

# 다른 방법

# 80%데이터를 제거 -c를 사용하지 않는 다른 방법
diamonds_0.8 <- nrow(diamonds) * 0.8
train <- diamonds[ c( (diamonds_0.8+1): nrow(diamonds)),]

nrow <- dim(ds)[1] * 0.8
nrow

ds2 <- ds[-c(1:nrow),]
dim(ds2)

ds2_filter <- ds2 %>% filter(cut == "Fair" & caret >= 1)
result <- max(ds2_filter$price)
print(result)


