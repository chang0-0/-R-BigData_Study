library(ggplot2)
library(dplyr)

data(diamonds)
ds <- diamonds
summary(ds)
str(ds)
 
sn <- sample(1: nrow(ds), size = nrow(ds)*0.8)
train <- ds[sn, ]
test <- ds[-sn, ]

print(train)
summary(test)

dim(test)


test_filter <- test %>% filter(cut == "Fair" & caret >= 1)
result <- max(test_filter$price)
print(result)

# 다른 방법
