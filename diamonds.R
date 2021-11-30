library(ggplot2)
library(dplyr)

# test 데이터 추출방법 1.
data(diamonds)
ds <- diamonds

nrow <- dim(ds)[1] * 0.8
ds_train <- ds[c(1:nrow),]

srt <- ds_train %>% arrange(desc(price))
ds_train100 <- head(srt , 100)
head(ds_train100)

result <- mean(ds_train100$price)
print(result)

### test데이터, training데이터 추출방법 2.
data(diamonds)
ds <- diamonds

sn <- sample(1:nrow(ds), size = nrow(ds)*0.8)
train <- ds[sn, ]
test <- ds[-sn, ]

print(train)
print(test)

srt <- train %>% arrange(desc(price))
print(srt)

train100 <- head(str , 100)
head(train100)

result <- mean(train100%price)
print(result)
